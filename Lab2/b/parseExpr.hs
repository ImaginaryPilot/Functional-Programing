import Data.Char
import Debug

{- parser for the grammar:
    E -> T E’
    E’ -> + T E’
    E’ -> - T E’
    E’ -> <empty string>
    T -> F T’
    T’ -> * F T’
    T’ -> / F T’
    T’ -> % F T’
    T’ -> <empty string>
    F -> ( E )
    F -> <integer>
-}

type Token  = String
type Tokens = [String]

lexer :: String -> Tokens
lexer [] = []
lexer str@(c:cs)
  | elem c "\n\t "  = lexer cs -- skip whitespace
  | elem c "*/+-%()"  = [c]:(lexer cs)
  | isDigit c    = takeWhile isDigit str : lexer(dropWhile isDigit str)
  | otherwise    = abort $ printf "illegal character '%c' found." c

parseE :: Tokens-> (Integer,Tokens)
parseE toks = parseE' $ parseT toks

parseE' :: (Integer,Tokens) -> (Integer,Tokens)
parseE' (acc, "+":toks) = 
    let (x, toks') = parseT toks
    in parseE' (acc + x, toks')
parseE' (acc, "-":toks) = 
    let (x, toks') = parseT toks
    in parseE' (acc - x, toks')
parseE' (acc,toks)     = (acc, toks)

parseT :: Tokens-> (Integer,Tokens)
parseT toks = parseT' $ parseF toks

parseT' :: (Integer,Tokens) -> (Integer,Tokens)
parseT' (acc, "*":toks) = 
    let (x, toks') = parseF toks
    in parseT' (acc * x, toks')
parseT' (acc, "/":toks) = 
    let (x, toks') = parseF toks
    in parseT' (div acc x, toks')
parseT' (acc, "%":toks) = 
    let (x, toks') = parseF toks
    in parseT' (mod acc x, toks')    
parseT' (acc,toks)     = (acc, toks)

parseF :: Tokens -> (Integer,Tokens)
parseF []     =  abort "error: unexpected end of input."
parseF ("(":toks) = 
    let (x, toks') = parseE toks
    in if not (null toks') && head toks' == ")"
        then (x, tail toks')
        else abort "error: missing parenthesis"
parseF ("-":toks) =
    let (x, toks') = parseF toks
    in (-x, toks')
parseF (tok:toks)
  | isDigit (head tok) =  (read tok, toks)
  | otherwise          =  abort $ printf "Error, unexpected '%s'." tok

evalParser :: String -> Integer
evalParser str = fst $ parseE $ lexer str