{-# LANGUAGE TypeApplications #-}
module MainSequenceModel where

import Conduit

import Control.Exception (Exception, throw)
import Control.Monad (liftM2, when)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isHorizontalSpace, isEndOfLine, endOfLine, double, decimal)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec

import Text.Printf


data ParseException = TopLevelException Int Int

instance Exception ParseException

instance Show ParseException where
  showsPrec _ (TopLevelException line col) = showString (printf "Failed to parse MS model at line %d, column %d" line col)


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

parseModel ::
  Monad m => ConduitT
    (Either ParseError (PositionRange, MSModelFormat))
    (Double, [(Double, [Int])])
    m
    ()
parseModel =
  mapC handleError .| filterC (not . isComment) .| unpack
  where handleError (Left (ParseError _ _ (Position line col _))) =
          throw $ TopLevelException line col
        handleError (Left DivergentParser) = error "Divergent Parser"
        handleError (Right (_, x)) = x

        unpack = do
          next <- await
          case next of
            Nothing -> return ()
            Just (SectionHeader feh _ _ _) -> section (feh, []) >> unpack
            _ -> unpack

        section s@(feh, ages) = do
          next <- await
          case next of
            Nothing -> doYield
            Just (AgeHeader a) -> do
              na <- age (a, [])
              section (feh, na:ages)
            Just l -> doYield >> leftover l
            where doYield = yield (feh, reverse ages)


        age (a, eeps) = do
          next <- await
          case next of
            Nothing -> doReturn
            Just (EEP eep mass filters) -> age (a, eep:eeps)
            Just l -> leftover l >> doReturn
            where doReturn = return (a, reverse eeps)
