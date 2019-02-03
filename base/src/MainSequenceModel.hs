module MainSequenceModel where

import Data.Char (isSpace)
import Data.Text
import Data.Attoparsec.Text

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

parseHeader = many1 $ choice [parseComment, parseFilters]
