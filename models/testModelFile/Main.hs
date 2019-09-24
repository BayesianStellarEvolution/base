module Main where

import Conduit

import Data.Either (lefts)
import Data.Set  (Set)
import Data.Text (Text)

import Options.Applicative

import Text.Printf

import Paths
import MainSequenceModel


loadModels :: (HasModelPath p) => p -> IO [Either (Double, Double, [(Double, Int)]) (([Text], Double, Double), Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "") .| lexModel .| parseModel .| mapC checkEeps

newtype ModelFile = MkModelFile { unModelFile :: String }

instance HasModelPath ModelFile where
  modelPath a _ = unModelFile a

main :: IO ()
main = do options <- execParser opts
          model <- loadModels options
          putStr "Parsed model successfully"

          let leftEeps = lefts model

          if null leftEeps
             then putStrLn ""
             else printLeftEeps leftEeps
  where
    opts = info (option (maybeReader (Just . MkModelFile)) (long "modelFile" <> help "Specify model archive") <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
    printLeftEeps eeps = do
        putStr . unlines . ("; however, at least the following EEPS are missing:" :) $ concatMap go eeps
        putStrLn "\nNote that only the first missing EEP for each age is printed."
      where go (feh, y, as) = let header = printf "\n  [Fe/H] = %.2f, Y = %.2f" feh y
                                  ages   = map (uncurry (printf "    Age = %.2f, EEP = %d")) as
                              in header : ages
