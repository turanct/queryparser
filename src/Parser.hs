{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser
    ( parseExpression
    ) where

import Query
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

type Parser a = Parsec Void String a

consumeSpaces :: Parser a -> Parser a
consumeSpaces p = space *> p

symbol :: String -> Parser String
symbol s = consumeSpaces (string s)

betweenParens :: Parser a -> Parser a
betweenParens = between (symbol "(") (symbol ")")

statementCharacter :: Char -> Bool
statementCharacter c = not $ elem c ['-', ' ', '\t', '\n', '\r', '(', ')']

term :: Parser Query
term = consumeSpaces (
           choice [ betweenParens expr
                  , Statement <$> takeWhile1P (Just "statement") statementCharacter
                  ] <?> "term"
      )

table = [ [ Prefix (Not <$ try (symbol "-"  )) ]
        , [ InfixL (And <$ try (symbol "AND")) ]
        , [ InfixL (Or  <$ try (symbol "OR" )) ]
        , [ InfixL (And <$ try (       " "  )) ]
        ]

expr = consumeSpaces (makeExprParser term table) <?> "expression"

parseExpression = runParser (expr) ""
