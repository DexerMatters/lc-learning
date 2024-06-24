module Main (main) where
import Text.Megaparsec (parse, parseTest)
import Prelude
import Lexing (lex)

path :: String
path = "/home/dexer/Projects/Haskell/Projects/lc-learning/examples/test.l"

main :: IO ()
main = do 
    str <- readFile path
    _   <- putStrLn $ "Readed as below\n" ++ str
    _   <- putStrLn "AST as below"
    parseTest Lexing.lex str
    

