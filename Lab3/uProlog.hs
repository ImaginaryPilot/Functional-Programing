import System.Environment
import System.IO
import Types
import Parser
import Analysis
import Clause
import Resolution

process :: String -> Clauses
process str = (resolveClauses.programToClauses.analyse.parseProgram) str

main = do
  args <- getArgs
  let reader = if args == [] then getContents else readFile (head args)
  text <- reader
  print (process text)
