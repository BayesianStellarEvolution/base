module MainSequenceModel where

import Conduit

import Control.Exception (Exception, throw)
import Control.Monad (liftM2, when)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isHorizontalSpace, isEndOfLine, endOfLine, double, decimal)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector.Unboxed (Vector)

import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Text.Printf


data MSModelException = LexException Position
                      | ParseException PositionRange

instance Exception MSModelException

instance Show MSModelException where
  showsPrec _ (LexException (Position line col _)) =
    showString $ printf "Failed to parse MS model at line %d, column %d" line col
  showsPrec _ (ParseException (PositionRange (Position line _ _) _)) =
    showString $ printf "Illegal lexeme in MS model on line %d" line


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


separator = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace


parseFilters =
  let parser = "%f" *> many1 (satisfy isHorizontalSpace *> takeWhile1 (not . liftM2 (||) isHorizontalSpace isEndOfLine)) <* endOfLine
  in Filters <$> parser <?> "MS Model filters"


parseComment =
  let parser = "#" *> skipWhile isHorizontalSpace *> takeTill isEndOfLine <* endOfLine
  in Comment <$> parser <?> "MS Model Comment"


parseEmptyLine = endOfLine *> pure (Comment "")


taggedDouble t = separator *> string t *> double


parseSectionHeader =
  let parser = SectionHeader <$> ("%s" *> feh)
                             <*> alphaFe
                             <*> lHp
                             <*> y
                             <*  endOfLine

  in parser <?> "MS Section header"
     where feh = taggedDouble "[Fe/H]=" <?> "FeH"
           alphaFe = taggedDouble "[alpha/Fe]="  <?> "alphaFe"
           lHp = taggedDouble "l/Hp=" <?> "lHp"
           y = taggedDouble "Y=" <?> "Y"


parseAgeHeader =
  let parser = AgeHeader <$> ("%a" *> logAge) <* endOfLine
  in parser <?> "MS Age header"
     where logAge = taggedDouble "logAge=" <?> "logAge"


parseEEP =
  let parser = EEP <$> (separator *> decimal)
                   <*> (separator *> double)
                   <*> (many1 (separator *> double))
                   <*  endOfLine
  in parser <?> "MS EEP"


lexModel ::
  Monad m => ConduitT
    ByteString
    (Either ParseError (PositionRange, MSModelFormat))
    m
    ()
lexModel = conduitParserEither (choice [parseEEP, parseAgeHeader, parseSectionHeader, parseComment, parseEmptyLine, parseFilters])
lexModel' ::
  MonadThrow m => ConduitT
    ByteString
    (PositionRange, MSModelFormat)
    m
    ()
lexModel' = conduitParser (choice [parseEEP, parseAgeHeader, parseSectionHeader, parseComment, parseEmptyLine, parseFilters])

data Age = Age !Double (Vector Int) (Vector Double)

instance Show Age where
  showsPrec _ (Age a _ _) = shows a

instance Eq Age where
  (Age a1 _ _) == (Age a2 _ _) = a1 == a2

instance Ord Age where
  compare = comparing (\(Age a _ _) -> a)

parseModel ::
  Monad m => ConduitT
    (Either ParseError (PositionRange, MSModelFormat))
    (Double, Set Age)
    m
    ()
parseModel =
  mapC handleError .| filterC (not . isComment . snd) .| unpack
  where handleError (Left (ParseError _ _ pos)) =
          throw $ LexException pos
        handleError (Left DivergentParser) = error "Divergent Parser"
        handleError (Right r) = r

        unpack = do
          filters <- concat <$> (header .| sinkList)

          let go = do
                next <- await
                case next of
                  Nothing -> return ()
                  Just (_, SectionHeader feh _ _ _) -> section feh >> go
                  _ -> go

          go

        header =
          let go = do
                next <- await
                case next of
                  Just (_, Filters fs) -> yield fs >> go
                  Just l -> leftover l
                  Nothing -> return ()
          in go

        section feh =
          let go ages = do
                next <- await
                case next of
                  Just (_, AgeHeader a) -> do
                    na <- age a
                    go $ na `S.insert` ages

                  Just l@(_, SectionHeader _ _ _ _) -> doYield ages >> leftover l
                  Just (pos, _)                     -> throw $ ParseException pos
                  Nothing                           -> doYield ages
          in go S.empty
            where doYield ages = yield (feh, ages)


        age a =
          let go eeps masses = do
                next <- await
                case next of
                  Just (_, EEP eep mass filters)    -> go (eeps `V.snoc` eep) (masses `V.snoc` mass)
                  Just l@(_, AgeHeader _)           -> leftover l >> doReturn eeps masses
                  Just l@(_, SectionHeader _ _ _ _) -> leftover l >> doReturn eeps masses
                  Just (pos, _)                     -> throw $ ParseException pos
                  Nothing                           -> doReturn eeps masses
          in go V.empty V.empty
            where doReturn eeps masses = return $ Age a eeps masses
