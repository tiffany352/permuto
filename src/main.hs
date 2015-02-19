import Lexer
import Eval
import Data.Map as Map
import Data.Maybe

read s = case readExprs s of
           Just (es, []) -> Just es
           _ -> Nothing

print :: Maybe [Expr] -> [Char]
print (Just st) = Lexer.showExprs . reverse $ st
print Nothing = "Error"

once c s = Main.print $ Main.read s >>= evalExprs c []

loop c = do
  line <- getLine
  putStrLn $ once c line
  loop c

main = do
  let c = Map.fromList [
           ("+", \(Number a:Number b:st) -> Just $ push (Number $ a+b) st),
           ("show", \(a:st) -> Just $ push (String $ show a) st),
           ("i", \(Quote cl:st) -> evalExprs c st cl),
           ("zap", \(v:st) -> Just st),
           ("dup", \(v:st) -> Just $ v:v:st),
           ("swap", \(a:b:st) -> Just $ b:a:st),
           ("dip", \(Quote a:b:st) -> fmap (push b) $ evalExprs c st a),
           ("unit", \(a:st) -> Just $ Quote [a]:st),
           ("cat", \(a:b:st) -> Just $ Quote (unquote b ++ unquote a):st),
           ("cons", \(a:b:st) -> Just $ Quote ([Quote $ unquote b] ++ unquote a):st)
          ]
  loop c
