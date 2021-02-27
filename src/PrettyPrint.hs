module PrettyPrint where

import Query

pretty :: Query -> String
pretty (And a b) = "(" ++ pretty a ++ " AND " ++ pretty b ++ ")"
pretty (Or a b) = "(" ++ pretty a ++ " OR " ++ pretty b ++ ")"
pretty (Not a) = "-" ++ pretty a
pretty (Statement a) = a

