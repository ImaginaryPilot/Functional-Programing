module Parser(parseProgram) where

import Types
import Lexer
import Error
import Data.Char

{- parser for the following gramar:
Prolog         -> Statement Prolog
Prolog         -> <empty>

Statement      -> '?-' Relation '.'
Statement      -> Relation Statement'
Statement'     -> ':-' RelationList '.'
Statement'     -> '.'

RelationList   -> Relation RelationList'
RelationList'  -> ',' Relation RelationList'
RelationList'  -> <empty>

Relation       -> Identifier Args

Args           -> '(' ArgList ')'

ArgList        -> Argument ArgList'
ArgList'       -> ',' Argument ArgList'
ArgList'       -> <empty>

Argument       -> <variable> | <constant>
-}

parseProgram :: String -> Program
parseProgram input = Program (parseProlog [] (lexer input))

parseProlog :: [(Statement,Int)] -> [(LexToken,Int)] -> [(Statement,Int)]
parseProlog xs [] = xs
parseProlog xs tokens = 
    let (x, rest) = statement tokens
    in parseProlog (xs ++ [x]) rest

statement :: [(LexToken,Int)] -> ((Statement,Int), [(LexToken,Int)])
statement ((QueryTok, line):tokens) =
    let (func, rest) = relation tokens
    in case rest of
        ((DotTok,_):rest') -> ((Query func, line), rest')
        ((tok, ln):_) -> expectedError ln (show tok)
        [] -> eofError
statement ((IdentTok name, line):tokens) = 
    let (func, rest) = relation ((IdentTok name, line):tokens)
    in statement' func line rest
statement ((tok, ln):_) = expectedError ln "a relation name (i.e. an identifier that start with a lower case letter)"
statement [] = eofError

statement' :: FuncApplication -> Int -> [(LexToken,Int)] -> ((Statement,Int), [(LexToken,Int)])
statement' headFunc line ((FollowsTok, _):tokens) =
    let (func, rest) = relationList tokens
    in case rest of
        ((DotTok,_):rest') -> ((Rule headFunc func, line), rest')
        ((tok, ln):_) -> expectedError ln (show tok)
        [] -> eofError
statement' headFunc line ((DotTok, _):tokens) = ((Fact headFunc, line), tokens)
statement' headFunc line ((tok, ln):_) = expectedError ln (show tok)
statement' headFunc line [] = eofError

relationList :: [(LexToken,Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList tokens = 
    let (func, rest) = relation tokens
        (otherFunc, rest') = relationList' rest
    in (func : otherFunc, rest')

relationList' :: [(LexToken,Int)] -> ([FuncApplication], [(LexToken,Int)])
relationList' ((CommaTok, _):tokens) = 
    let (func, rest) = relation tokens
        (otherFunc, rest') = relationList' rest
    in (func : otherFunc, rest')
relationList' tokens = ([], tokens)

relation :: [(LexToken,Int)] -> (FuncApplication, [(LexToken,Int)])
relation ((IdentTok name, ln):tokens)
    | not (null name) && isLower (head name) =
        let (argsList, rest) = args tokens
        in (FuncApp name argsList, rest)
    | otherwise = expectedError ln "a relation name (i.e. an identifier that start with a lower case letter)"
relation ((tok, ln):_) = expectedError ln "a relation name (i.e. an identifier that start with a lower case letter)"
relation [] = eofError

args :: [(LexToken,Int)] -> ([Argument], [(LexToken,Int)])
args ((LparTok, _):tokens) = 
    let (argsList, rest) = argList tokens
    in case rest of
        ((RparTok,_):rest') -> (argsList, rest')
        ((tok, ln):_) -> expectedError ln "a closing parenthesis"
        [] -> eofError
args ((tok, ln):_) = expectedError ln "an opening parenthesis"
args [] = eofError

argList :: [(LexToken,Int)] -> ([Argument], [(LexToken,Int)])
argList tokens = 
    let (firstArg, rest) = argument tokens
        (otherArgs, rest') = argList' rest
    in (firstArg : otherArgs, rest')

argList' :: [(LexToken,Int)] -> ([Argument], [(LexToken,Int)])
argList' ((CommaTok, _):tokens) = 
    let (firstArg, rest) = argument tokens
        (otherArgs, rest') = argList' rest
    in (firstArg : otherArgs, rest')
argList' tokens = ([], tokens)

argument :: [(LexToken,Int)] -> (Argument, [(LexToken,Int)])
argument ((IdentTok name, _):tokens) = (Const name, tokens)
argument ((VarTok name, _):tokens) = (Arg name, tokens)
argument ((tok, ln):_) = expectedError ln "a literal"
argument [] = eofError