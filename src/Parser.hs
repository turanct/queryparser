{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser
    ( parseExpression
    ) where

import Query
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

statement :: Parser Query
statement = Statement <$> takeWhile1P (Just "statement") statementCharacter
    where statementCharacter c = not $ elem c ['-', ' ', '\t', '\n', '\r', '(', ')']

field :: Parser String
field = takeWhile1P (Just "field") fieldChar <* symbol ":"
    where fieldChar c = elem c $ ['a'..'z'] ++ ['.']

fieldRange :: Parser Query
fieldRange = FieldRange <$> field <*> range
    where
          range :: Parser Range
          range = do
              symbol "["
              r <- choice [try between, try greaterThan, try lessThan]
              symbol "]"
              return r

          between :: Parser Range
          between = do
              a <- number
              symbol "TO "
              b <- number
              return $ Between a b

          greaterThan :: Parser Range
          greaterThan = do
              a <- number
              symbol "TO *"
              return $ GreaterThan a

          lessThan :: Parser Range
          lessThan = do
              symbol "* TO "
              a <- number
              return $ LessThan a

          number :: Parser Int
          number = fmap read $ some digitChar

fieldValue = FieldValue <$> field <*> (choice [try quotedValue, value])
    where
        quotedValue :: Parser String
        quotedValue = between (symbol "\"") (symbol "\"") (takeWhileP (Just "quotedValue") quotedChar)

        quotedChar c = not $ elem c ['"']

        value :: Parser String
        value = takeWhile1P (Just "value") valueChar

        valueChar c = not $ elem c ['-', ' ', '\t', '\n', '\r', '(', ')']

term :: Parser Query
term = consumeSpaces (
           choice [ try (betweenParens expr)
                  , try fieldRange
                  , try fieldValue
                  , statement
                  ] <?> "term"
      )

implicitAnd :: Parser String
implicitAnd = string " " <* notFollowedBy (space *> string ")")

table = [ [ Prefix (Not <$ try (symbol "-"  )) ]
        , [ InfixL (And <$ try (symbol "AND")) ]
        , [ InfixL (Or  <$ try (symbol "OR" )) ]
        , [ InfixL (And <$ try (implicitAnd )) ]
        ]

expr = consumeSpaces (makeExprParser term table) <?> "expression"

parseExpression = parseOneLine . unLine
    where
        parseOneLine = runParser (expr) ""

        unLine :: String -> String
        unLine = unwords . lines
