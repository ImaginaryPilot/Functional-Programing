module Unify (mgu, applyUnifier) where

import Types

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier
mgu (FuncApp f1 args1) (FuncApp f2 args2)
    | f1 /= f2 = Nothing
    | length args1 /= length args2 = Nothing
    | otherwise = unifyArgs args1 args2

unifyArgs :: [Argument] -> [Argument] -> Maybe Unifier
unifyArgs [] [] = Just []
unifyArgs (a:as) (b:bs) = 
    case unifyArg a b of
        Nothing -> Nothing
        Just u1 -> 
            case unifyArgs (map (applyArg u1) as) (map (applyArg u1) bs) of
                Nothing -> Nothing
                Just u2 -> 
                    let updatedArgs = map (\(v,a)-> (v, applyArg u2 a)) u1
                    in Just (updatedArgs ++ u2)
unifyArgs _ _ = Nothing

unifyArg :: Argument -> Argument -> Maybe Unifier
unifyArg (Const a) (Const b)
    | a == b = Just []
    | otherwise = Nothing
unifyArg (Arg x) arg = Just [(x, arg)]
unifyArg arg (Arg x) = Just [(x, arg)]

applyUnifier :: Unifier -> FuncApplication -> FuncApplication
applyUnifier subs (FuncApp name args) = 
    FuncApp name (map (applyArg subs) args)

applyArg :: Unifier -> Argument -> Argument
applyArg subs (Const x) = (Const x)
applyArg subs (Arg x) = 
    case lookup x subs of
        Just replace -> replace
        Nothing -> (Arg x)