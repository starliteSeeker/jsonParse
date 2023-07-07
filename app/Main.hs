{-# LANGUAGE LambdaCase #-}

module Main where

import Json
import System.Environment
import System.IO
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [] -> putStrLn $ "Usage: " ++ name ++ " FILE"
    f : _ ->
      readFile f
        >>= ( \case
                Left e -> putStrLn $ errorBundlePretty e
                Right v -> print v
            )
          . parseJson f
