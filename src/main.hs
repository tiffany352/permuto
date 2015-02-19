import Lexer
import Eval
import Data.Map as Map

read s = case readExprs s of
           Just (es, []) -> Just es
           _ -> Nothing

once c s = show $ Main.read s >>= evalExprs c []

loop c = do
  line <- getLine
  putStrLn $ once c line
  loop c

main = do
  let c = Map.fromList [
           ("+", \(Number a:Number b:st) -> push (Number $ a+b) st)
          ]
  loop c
