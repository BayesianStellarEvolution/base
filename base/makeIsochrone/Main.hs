module Main where

import Conduit

import Control.Monad (when)

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec
import Data.Conduit.Lzma
import Data.Either (isRight)

import MainSequenceModel

main :: IO ()
main = do
  t <- runConduitRes ( sourceFile "/home/elliot/projects/base-models/dsed/dsed_new.model.xz"
                    .| decompress Nothing
                    .| conduitParserEither (choice [parseEEPs, parseAgeHeader, parseSectionHeader, parseComment, parseEmptyLine, parseFilters])
                    .| filterC (either (const True) (not . isComment . snd))
                    .| lastC
                     )

  print t


go ::
  ConduitT
    ByteString (PositionRange, MSModelFormat) (ResourceT IO) ()
go = do
  header <- sinkParser parseFileHeader

  let headerWithoutComments = filter isFilters header
      records = length headerWithoutComments

  when (records == 0) $ fail $ "MS Model - malformed header: " ++ show records ++ " filter specifications"

  let filters = concatMap (\(Filters f) -> f) headerWithoutComments
      nFilters = length filters

  conduitParser (choice [parseEEPs, parseAgeHeader, parseSectionHeader, parseComment, parseEmptyLine])


  --  .| filterC (either (const True) (not . isComment . snd))


-- Lex file .| filter comments .| recursive parser
