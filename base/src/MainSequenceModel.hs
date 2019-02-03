module MainSequenceModel where

import Data.Text
import qualified Data.Attoparsec.Text as A

data Model = Model { filters :: [Text] }

parseFilters :: Text -> [Text]
parseFilters t = ["U"]
