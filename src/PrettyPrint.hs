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

prettyIndent :: Int -> Query -> String
prettyIndent i (And a b) = indent i ++ "(\n" ++ prettyIndent (i + 1) a ++ "\n" ++ indent i ++ "AND\n" ++ prettyIndent (i + 1) b ++ "\n" ++ indent i ++ ")"
prettyIndent i (Or a b) = indent i ++ "(\n" ++ prettyIndent (i + 1) a ++ "\n" ++ indent i ++ "OR\n" ++ prettyIndent (i + 1) b ++ "\n" ++ indent i ++ ")"
prettyIndent i (Not (FieldValue field value)) = indent i ++ "-" ++ field ++ ":\"" ++ value ++ "\""
prettyIndent i (Not (FieldRange field range)) = indent i ++ "-" ++ field ++ ":" ++ prettyRange range
prettyIndent i (Not (Statement a)) = indent i ++ "-" ++ a
prettyIndent i (Not a) = indent i ++ "-(\n" ++ prettyIndent (i + 1) a ++ "\n" ++ indent i ++ ")"
prettyIndent i s = indent i ++ pretty s

indent :: Int -> String
indent x = take (x * 4) (repeat ' ')
