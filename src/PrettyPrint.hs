module PrettyPrint where

import Query

pretty :: Query -> String
pretty (And a b) = "(" ++ pretty a ++ " AND " ++ pretty b ++ ")"
pretty (Or a b) = "(" ++ pretty a ++ " OR " ++ pretty b ++ ")"
pretty (Not a) = "-" ++ pretty a
pretty (FieldValue field value) = field ++ ":\"" ++ value ++ "\""
pretty (FieldRange field range) = field ++ ":" ++ prettyRange range
pretty (Statement a) = a

prettyRange :: Range -> String
prettyRange (Between a b) = "[" ++ show a ++ " TO " ++ show b ++ "]"
prettyRange (GreaterThan a) = "[" ++ show a ++ " TO *]"
prettyRange (LessThan a) = "[* TO " ++ show a ++ "]"
