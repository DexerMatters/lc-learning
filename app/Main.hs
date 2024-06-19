module Main (main) where
import Text.Megaparsec (parseTest)
import Prelude
import Lexing (pTerm)

path :: String
path = "/home/dexer/Projects/Haskell/Projects/lc-learning/examples/test.l"

main :: IO ()
main = do
    str <- readFile path
    _   <- putStrLn $ "Readed as below\n" ++ str
    _   <- putStrLn "AST as below"
    parseTest (pTerm 0) str


