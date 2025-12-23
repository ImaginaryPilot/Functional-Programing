module Resolution (resolveClauses) where

import Types
import Unify
import Data.Maybe
import Data.List 

resolveClauses :: Clauses -> Clauses
resolveClauses clauses = loopResolve clauses

loopResolve :: Clauses -> Clauses
loopResolve clauses =
    let newClauses = resolve clauses
        unique = nub (clauses ++ newClauses)
        checker = filter (`notElem` clauses) unique
    in if null checker
        then unique
        else loopResolve unique


resolve :: Clauses -> Clauses
resolve clauses = pairResolver (pairClauses clauses) []
    where
        pairResolver [] xs = xs
        pairResolver ((c1, c2):cs) xs = 
            let c1' = renameClause 1 c1
                c2' = renameClause 2 c2
                newClause = resolveLiterals c1' c2'
            in pairResolver cs (xs++newClause)
    
applyUnifierToLiteral :: Unifier -> (FuncApplication, Bool) -> (FuncApplication, Bool)
applyUnifierToLiteral u (fa, b) = (applyUnifier u fa, b)

pairClauses :: Clauses -> [(Clause, Clause)]
pairClauses [] = []
pairClauses (c:cs) = [(c, c1) | c1 <- cs] ++ pairClauses cs

checkLiterals :: Clause -> Clause -> [Unifier]
checkLiterals c1 c2 = mapMaybe (\(lit1, lit2)-> mgu lit1 lit2) [(l1, l2) | (l1, b1)<-c1, (l2, b2)<-c2, b1/=b2]

resolveLiterals :: Clause -> Clause -> [Clause]
resolveLiterals c1 c2 = do
    l1@(_,b1) <- c1
    l2@(_,b2) <- c2
    if b1 /= b2
        then case mgu (fst l1) (fst l2) of 
                Nothing -> []
                Just u -> 
                    let both = removeLiteral c1 l1 ++ removeLiteral c2 l2
                        resolvedClause = map (applyUnifierToLiteral u) both
                    in [resolvedClause]
        else []

removeLiteral :: Clause -> (FuncApplication, Bool) -> Clause
removeLiteral c l = filter (\lit -> not(sameLiteral lit l)) c

sameLiteral :: (FuncApplication, Bool) -> (FuncApplication, Bool) -> Bool
sameLiteral (FuncApp n1 args1, b1) (FuncApp n2 args2, b2) = n1 == n2 && b1 == b2 && sameArgs args1 args2

sameArgs :: [Argument] -> [Argument] -> Bool
sameArgs [] [] = True
sameArgs (a:as) (b:bs) = sameArg a b && sameArgs as bs
    where 
        sameArg (Const c1) (Const c2) = c1 == c2
        sameArg (Arg a1) (Arg a2) = a1 == a2
        sameArg _ _ = False
sameArgs _ _ = False

renameClauses :: Int -> Clauses -> Clauses
renameClauses _ [] = []
renameClauses id (c:cs) = renamed : renameClauses (id+1) cs
    where
        renamed = renameClause id c

renameClause :: Int -> Clause -> Clause
renameClause id clause = map rename clause
    where 
        rename (FuncApp name args, b) = (FuncApp name (map renameArg args), b)
        renameArg (Arg var) = Arg (var ++ show id)
        renameArg (Const name) = Const name