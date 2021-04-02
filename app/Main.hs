module Main where

import Parser
import PrettyPrint
import Data.Either

main :: IO ()
main = interact f
  where
    f :: String -> String
    f s = fromRight "error" $ fmap (prettyIndent 0) $ parseExpression $ unLine s

    unLine :: String -> String
    unLine = unwords . lines

