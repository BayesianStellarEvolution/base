module MainSequenceModel where

import Control.Monad (liftM2, when)
import Data.Text (Text)
import Data.Attoparsec.Text


data MSModel = MSModel { filters :: [Text]
                       , sections :: [MSModelFormat] }
             deriving (Show, Eq)


data MSModelFormat = Filters [Text]
                   | SectionHeader Double Double Double Double
                   | AgeHeader Double
                   | EEP Int Double [Double]
                   | Comment Text
                   deriving (Show, Eq)


isFilters (Filters _) = True
isFilters _           = False

isComment (Comment _) = True
isComment _           = False


isSpace = inClass " \t"
isNewline = inClass "\n\r"


separator = satisfy isSpace *> skipWhile isSpace


parseFilters =
  let parser = "%f" *> many1 (satisfy isSpace *> takeWhile1 (not . liftM2 (||) isSpace isNewline)) <* endOfLine
  in Filters <$> parser <?> "MS Model filters"


parseComment =
  let parser = "#" *> skipWhile isSpace *> takeTill (inClass "\n\r") <* endOfLine
  in Comment <$> parser <?> "MS Model Comment"


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
  let parser = AgeHeader <$> ("%a" *> logAge)
  in parser <?> "MS Age header"
     where logAge = taggedDouble "logAge=" <?> "logAge"


parseEEP c =
  let parser = EEP <$> (separator *> decimal)
                   <*> (separator *> double)
                   <*> (count c (separator *> double))
                   <*  endOfLine
  in parser <?> "MS EEP"


parseModel = do
  header <- parseFileHeader

  let headerWithoutComments = filter isFilters header
      records = length headerWithoutComments

  when (records == 0) $ fail $ "MS Model - malformed header: " ++ show records ++ " filter specifications"

  let filters = concatMap (\(Filters f) -> f) headerWithoutComments

  return $ MSModel filters []
