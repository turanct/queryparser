module Query where

data Query = And Query Query
           | Or Query Query
           | Not Query
           | FieldValue String String
           | FieldRange String Range
           | Statement String
           deriving (Show)

data Range = Between Int Int
           | GreaterThan Int
           | LessThan Int
           deriving (Show)
