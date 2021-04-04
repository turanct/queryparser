module Main where

import Parser
import Transform
import PrettyPrint
import Data.Either

main :: IO ()
main = interact f
  where
    f :: String -> String
    f s = fromRight "error" $ fmap (prettyIndent 0) $ fmap orOutside $ parseExpression $ unLine s

    unLine :: String -> String
    unLine = unwords . lines
