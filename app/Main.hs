module Main where

import Parser
import Transform
import PrettyPrint
import Data.Either
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    userInput <- getContents

    let show' = case (elem "--pretty" args) of
            True -> (prettyIndent 0)
            False -> pretty

    let normalize = case (elem "--dnf" args) of
            True -> orOutside
            False -> id

    let output = fromRight "error" $ fmap show' $ fmap normalize $ parseExpression userInput

    putStrLn output
