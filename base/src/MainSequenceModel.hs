module MainSequenceModel where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Attoparsec.Text

data MSModel = MSModel { filters :: [Text] }
             deriving (Show, Eq)

data MSModelFormat = Filters [Text]
                   | Comment Text
                   deriving (Show, Eq)

isFilters (Filters _) = True
isFilters _           = False

parseFilters =
  let parser = "%f" *> many1 (space *> takeWhile1 (not . isSpace)) <* endOfLine
  in Filters <$> parser <?> "MS Model filters"

parseComment =
  let parser = "#" *> skipWhile isSpace *> takeTill (inClass "\n\r") <* endOfLine
  in Comment <$> parser <?> "MS Model Comment"

parseHeader =
  let parser = many1 $ choice [parseComment, parseFilters]
  in parser <?> "MS Model header"

parseModel = do
  header <- parseHeader

  let headerWithoutComments = filter isFilters header
      records = length headerWithoutComments

  when (records /= 1) $ fail $ "MS Model - malformed header: " ++ show records ++ " filter specifications"

  let (Filters filters) = head headerWithoutComments

  return $ MSModel filters
