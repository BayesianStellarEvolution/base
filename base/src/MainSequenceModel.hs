module MainSequenceModel where

import Control.Monad (liftM2, when)
import Data.Text (Text)
import Data.Attoparsec.Text


data MSModel = MSModel { filters :: [Text] }
             deriving (Show, Eq)


data MSModelFormat = Filters [Text]
                   | SectionHeader Double Double
                   | Comment Text
                   deriving (Show, Eq)


isFilters (Filters _) = True
isFilters _           = False


isSpace = inClass " \t"
isNewline = inClass "\n\r"


parseFilters =
  let parser = "%f" *> many1 (satisfy isSpace *> takeWhile1 (not . liftM2 (||) isSpace isNewline)) <* endOfLine
  in Filters <$> parser <?> "MS Model filters"


parseComment =
  let parser = "#" *> skipWhile isSpace *> takeTill (inClass "\n\r") <* endOfLine
  in Comment <$> parser <?> "MS Model Comment"


parseFileHeader =
  let parser = many1 $ choice [parseComment, parseFilters]
  in parser <?> "MS Model header"


parseSectionHeader =
  let parser = SectionHeader <$> ("%s" *> feh)
                             <*> alphaFe

  in parser <?> "MS Section header"
     where feh = skipWhile isSpace *> "[Fe/H]=" *> double
           alphaFe = skipWhile isSpace *> "[alpha/Fe]=" *> double

parseModel = do
  header <- parseFileHeader

  let headerWithoutComments = filter isFilters header
      records = length headerWithoutComments

  when (records == 0) $ fail $ "MS Model - malformed header: " ++ show records ++ " filter specifications"

  let filters = concatMap (\(Filters f) -> f) headerWithoutComments

  return $ MSModel filters
