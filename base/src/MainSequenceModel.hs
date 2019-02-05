module MainSequenceModel where

import Control.Monad (liftM2, when)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isHorizontalSpace, isEndOfLine, endOfLine, double, decimal)


data MSModel = MSModel { filters :: [ByteString]
                       , sections :: [MSModelFormat] }
             deriving (Show, Eq)


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


parseFileHeader =
  let parser = many1 $ choice [parseComment, parseFilters]
  in parser <?> "MS Model header"


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


parseEEPs =
  let parser = EEP <$> (separator *> decimal)
                   <*> (separator *> double)
                   <*> (many1 (separator *> double))
                   <*  endOfLine
  in parser <?> "MS EEP"


parseModel = do
  header <- parseFileHeader

  let headerWithoutComments = filter isFilters header
      records = length headerWithoutComments

  when (records == 0) $ fail $ "MS Model - malformed header: " ++ show records ++ " filter specifications"

  let filters = concatMap (\(Filters f) -> f) headerWithoutComments
      nFilters = length filters

  rest <- filter (not . isComment) <$> many' (choice [parseEEPs, parseAgeHeader, parseSectionHeader, parseComment, parseEmptyLine ])

  endOfInput <?> "MS Model end of file"

  return rest
