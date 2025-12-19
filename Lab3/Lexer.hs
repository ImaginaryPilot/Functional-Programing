module Lexer(LexToken(..),lexer) where
import Data.Char
import Error

data LexToken = DotTok   | CommaTok | FollowsTok
              | QueryTok | LparTok  | RparTok
              | IdentTok String
              | VarTok String
  deriving Eq

instance Show LexToken where
  show DotTok            = "."
  show CommaTok          = ","
  show FollowsTok        = ":-"
  show QueryTok          = "?-"
  show LparTok           = "("
  show RparTok           = ")"
  show (IdentTok name)   = "<id:" ++ name ++ ">"
  show (VarTok name)     = "<var:" ++ name ++ ">"

lexer :: String -> [(LexToken,Int)]    -- The Int is the line number in the source code
lexer input = lex' 1 input

lex' :: Int -> String -> [(LexToken,Int)]
lex' _ [] = []
lex' line (c:cs)
-- Whitespace, tab, enter and comments
  | c == ' ' || c == '\t' = lex' line cs
  | c == '\n' = lex' (line+1) cs
  | c == '%' = lex' line (dropWhile (/= '\n') cs)

-- Two character input
  | c == ':' && not (null cs) && (head cs) == '-' = (FollowsTok, line) : lex' line (tail cs)
  | c == '?' && not (null cs) && (head cs) == '-' = (QueryTok, line) : lex' line (tail cs)

-- One Character input
  | c == '.' = (DotTok, line) : lex' line cs
  | c == ',' = (CommaTok, line) : lex' line cs
  | c == '(' = (LparTok, line) : lex' line cs
  | c == ')' = (RparTok, line) : lex' line cs

-- Variables and Identifiers
  | isAlpha c = 
      let (name, rest) = span isAlphaNum (c:cs)
          tok = if isUpper c then VarTok name else IdentTok name
      in (tok, line) : lex' line rest

-- Error
  | otherwise = lexError line c