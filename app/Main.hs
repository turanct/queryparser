module Main where

import Parser
import PrettyPrint
import Data.Either

main :: IO ()
main = interact f
  where
    f :: String -> String
    f s = fromRight "error" $ fmap pretty (parseExpression s)

