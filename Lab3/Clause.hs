module Clause (programToClauses) where

import Types

-- Convert a Program into Clauses
programToClauses :: Program -> Clauses
programToClauses (Program stmts) = mapMaybe (statementToClause . fst)  stmts

statementToClause :: Statement -> Maybe Clause
statementToClause (Query _) = Nothing
statementToClause (Fact fact) = Just [(fact, True)]
statementToClause (Rule func otherFunc) =
    Just (map (\f -> (f, False)) otherFunc ++ [(func, True)])

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = 
    case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs