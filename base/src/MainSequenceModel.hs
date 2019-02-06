module MainSequenceModel where

import Conduit (MonadThrow, ConduitT, await, yield)

import Control.Monad (liftM2, when)

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isHorizontalSpace, isEndOfLine, endOfLine, double, decimal)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec

import Text.Printf


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
    Double
    m
    ()
parseModel =
  loop
  where loop = do
          next <- await
          case next of
            Nothing -> return ()
            Just x  -> handleError x >>= unpack >> loop
        handleError (Left e@(ParseError ctxt msg (Position line col _))) =
          fail $ printf "Failed to parse MS model at line %d, column %d" line col
        handleError (Right (_, x)) = return x
        unpack (SectionHeader feh _ _ y) = yield feh
        unpack _ = return ()
