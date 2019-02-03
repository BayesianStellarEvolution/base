module MainSequenceModel where

import Data.Char (isSpace)
import Data.Text
import Data.Attoparsec.Text

data Model = Model { filters :: [Text] }


parseFilters = string "%f" *> many1 (space *> takeWhile1 (not . isSpace)) <* endOfLine
