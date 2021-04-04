module Transform where

import Query

orOutside :: Query -> Query
orOutside = toDNF

-- Negation Normal Form
toNNF :: Query -> Query
toNNF (Statement a) = Statement a
toNNF (FieldValue a b) = FieldValue a b
toNNF (FieldRange a b) = FieldRange a b
toNNF (Not (Statement a)) = Not (Statement a)
toNNF (Not (FieldValue a b)) = Not (FieldValue a b)
toNNF (Not (FieldRange a b)) = Not (FieldRange a b)
toNNF (Not (Not a)) = a

toNNF (And a b) = And (toNNF a) (toNNF b)
toNNF (Not (And a b)) = toNNF $ Or (Not a) (Not b)

toNNF (Or a b) = Or (toNNF a) (toNNF b)
toNNF (Not (Or a b)) = toNNF $ And (Not a) (Not b)

-- Disjuntive Normal Form
toDNF :: Query -> Query
toDNF = toDNF' . toNNF
  where
    toDNF' :: Query -> Query
    toDNF' (And a b) = dist (toDNF' a) (toDNF' b)
    toDNF' (Or a b) = Or (toDNF' a) (toDNF' b)
    toDNF' a = a

    dist :: Query -> Query -> Query
    dist (Or a b) c = Or (a `dist` c) (b `dist` c)
    dist a (Or b c) = Or (a `dist` b) (a `dist` c)
    dist a b = And a b
