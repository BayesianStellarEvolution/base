{-# LANGUAGE TypeApplications #-}
module MainSequenceModel where

import Conduit

import Control.Exception (Exception, throw)
import Control.Monad (liftM2, when)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isHorizontalSpace, isEndOfLine, double, decimal, char, notChar)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector.Unboxed (Vector)

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Text.Printf

import Data.List (intersperse)


data MSModelException = LexException         [String] Position
                      | ParseException       PositionRange
                      | FilterCountException PositionRange Int Int

instance Exception MSModelException

instance Show MSModelException where
  showsPrec _ (LexException context (Position line col _)) =
    showString $ printf "Failed to lex main sequence model at line %d, column %d\nContext: %s"
                     line col $ concat $ intersperse " > " context
  showsPrec _ (ParseException (PositionRange (Position line _ _) _)) =
    showString $ printf "Illegal lexeme in main sequence model on line %d" line
  showsPrec _ (FilterCountException (PositionRange (Position line _ _) _) nFilters eepFilters) =
    showString $ printf "Incorrect number of filters on line %d. Expected %d, found %d." line nFilters eepFilters


data MSModelFormat = Filters [ByteString]
                   | SectionHeader Double Double Double Double
                   | AgeHeader Double
                   | EEP Int Double [Double]
                   | Comment ByteString
                   deriving (Show, Eq)


isFilters (Filters _) = True
isFilters _           = False

isComment (Comment _) = True
isComment _           = False


separator = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace <?> "required spacing"


endOfLine = AP.endOfLine <?> "end of line"


parseFilters =
  let parser = many1 (satisfy isHorizontalSpace *> takeWhile1 (not . liftM2 (||) isHorizontalSpace isEndOfLine)) <* endOfLine
  in Filters <$> parser <?> "Filters"


parseComment =
  let parser = skipWhile isHorizontalSpace *> takeTill isEndOfLine <* endOfLine
  in Comment <$> parser <?> "Comment"


parseEmptyLine :: Parser MSModelFormat
parseEmptyLine = pure (Comment "")


taggedDouble t = separator *> string t *> double


parseHeader = do
  a <- eitherP "a" (eitherP "s" "f")
  case a of
    Left _ -> parseAgeHeader
    Right (Left _) -> parseSectionHeader
    Right (Right _) -> parseFilters

parseSectionHeader =
  let parser = SectionHeader <$> feh
                             <*> alphaFe
                             <*> lHp
                             <*> y
                             <*  endOfLine

  in parser <?> "Section header"
     where feh = taggedDouble "[Fe/H]=" <?> "FeH"
           alphaFe = taggedDouble "[alpha/Fe]="  <?> "alphaFe"
           lHp = taggedDouble "l/Hp=" <?> "lHp"
           y = taggedDouble "Y=" <?> "Y"


parseAgeHeader =
  let parser = AgeHeader <$> logAge <* endOfLine
  in parser <?> "Age header"
     where logAge = taggedDouble "logAge=" <?> "logAge"


parseEEP =
  let parser = EEP <$> (skipWhile isHorizontalSpace *> decimal <?> "EEP")
                   <*> (separator *> double <?> "Mass")
                   <*> (many1 (separator *> double) <?> "Filters")
                   <*  endOfLine
  in parser <?> "EEP"


-- | Lex a Main Sequence model
--
-- Strange goings-on with eitherP here are to force the parser to make choices early
-- This is strictly necessary to get useful error messages, and has a slight performance boost (over choose) as a bonus
lexModel ::
  Monad m => ConduitT
    ByteString
    (Either ParseError (PositionRange, MSModelFormat))
    m
    ()
lexModel = conduitParserEither $ do
  c <- eitherP (char '%') (eitherP (eitherP (char '#') (char '\n')) (pure True))
  case c of
    Right (Right _) -> parseEEP
    Left _ -> parseHeader
    Right (Left (Left _)) -> parseComment
    Right (Left (Right _)) -> parseEmptyLine



data Age = Age !Double (Vector Int) (Vector Double) [Vector Double] deriving (Eq, Show)

instance Ord Age where
  compare = comparing (\(Age a _ _ _) -> a)

newtype PrettyAge = PrettyAge Age
  deriving (Eq, Ord)

instance Show PrettyAge where
  showsPrec _ (PrettyAge (Age a _ _ _)) = shows a

parseModel ::
  Monad m => ConduitT
    (Either ParseError (PositionRange, MSModelFormat))
    (Double, Set Age)
    m
    ()
parseModel =
  mapC handleError .| filterC (not . isComment . snd) .| unpack
  where handleError (Left p@(ParseError context _ pos)) =
          throw $ LexException context pos
        handleError (Left DivergentParser) = error "Divergent Parser"
        handleError (Right r) = r

        unpack = do
          filters <- concat <$> (header .| sinkList)

          let nFilters = length filters
              go = do
                next <- await
                case next of
                  Nothing -> return ()
                  Just (_, SectionHeader feh _ _ _) -> section nFilters feh >> go
                  _ -> go

          go

        header =
          let go = do
                next <- await
                case next of
                  Just (_, Filters fs) -> yield fs >> go
                  Just l               -> leftover l
                  Nothing              -> return ()
          in go

        section nFilters feh =
          let go ages = do
                next <- await
                case next of
                  Just (_, AgeHeader a) -> do
                    na <- age nFilters a
                    go $ na `S.insert` ages

                  Just l@(_, SectionHeader _ _ _ _) -> doYield ages >> leftover l
                  Just (pos, _)                     -> throw $ ParseException pos
                  Nothing                           -> doYield ages
          in go S.empty
            where doYield ages = yield (feh, ages)


        age nFilters a =
          let go eeps masses fs = do
                next <- await
                case next of
                  Just (pos, EEP eep mass filters)  -> do
                    let eepFilters = length filters

                    when (eepFilters /= nFilters) $ throw $ FilterCountException pos nFilters eepFilters

                    go (eeps `V.snoc` eep) (masses `V.snoc` mass) $ zipWith V.snoc fs filters

                  Just l@(_, AgeHeader _)           -> leftover l >> doReturn eeps masses fs
                  Just l@(_, SectionHeader _ _ _ _) -> leftover l >> doReturn eeps masses fs
                  Just (pos, _)                     -> throw $ ParseException pos
                  Nothing                           -> doReturn eeps masses fs
          in go V.empty V.empty $ replicate nFilters (V.empty @Double)
            where doReturn eeps masses fs = return $ Age a eeps masses fs
