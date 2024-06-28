module Main (main) where
import Text.Megaparsec (parse, parseTest)
import Prelude
import Lexing (lex)
import Typing (typeof)
import Utils.EvalEnv (runEval, check)
import Context (indexProc)
import Subtyping (makeSubGraph)
import Desugar (desugarProc)

path :: String
path = "/home/dexer/Projects/Haskell/Projects/lc-learning/examples/test.l"

main :: IO ()
main = do 
    str <- readFile path
    _   <- putStrLn $ "**Readed as below\n" ++ str
    case parse Lexing.lex "" str of
        Right ast -> 
               putStrLn "\n**AST as below"
            >> print (runEval ast check)
            >> putStrLn "\n**Typeof as below"
            >> print (runEval ast (
                   desugarProc 
                >> indexProc 
                >> makeSubGraph 
                >> typeof
                ))
        Left err -> print err