module Query where

data Query = And Query Query
           | Or Query Query
           | Not Query
           | Statement String
           deriving (Show)

