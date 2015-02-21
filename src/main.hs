import Lexer
import Eval
import Data.Map as Map
import Data.Maybe
import Control.Monad as Monad

read :: [Char] -> Result [Expr]
read s = case readExprs s of
           Right (es, []) -> Right es
           Left x -> Left x

print :: Result [Expr] -> [Char]
print (Right st) = Lexer.showExprs . reverse $ st
print (Left e) = e

once c s = Main.print $ Main.read s >>= evalExprs c []

loop c = do
  line <- getLine
  putStrLn $ once c line
  loop c

libAdd (a:b:st) =
    maybeResult "Expected number"
              (Monad.liftM2 (+) (number a) (number b)
               >>= Just . (flip push st) . Number)
libAdd _ = Left "add requires two parameters"
libShow (a:st) = Right $ push (String $ show a) st
libShow _ = Left "show requires a parameter"
libI c (Quote q:st) = evalExprs c st q
libI c _ = Left "Quoted parameter required"
libZap (v:st) = Right st
libZap _ = Left "Can't zap an empty stack"
libDup (v:st) = Right $ v:v:st
libDup _ = Left "Can't dup an empty stack"
libSwap (a:b:st) = Right $ b:a:st
libSwap _ = Left "swap requires two parameters"
libDip c (Quote q:b:st) = fmap (push b) $ evalExprs c st q
libDip c (Quote q:st) = Left "dip requires two parameters"
libDip c _ = Left "First argument to dip must be quoted"
libUnit (a:st) = Right $ Quote [a] : st
libUnit _ = Left "Can't unit empty stack"
libCat (a:b:st) = Right $ Quote (unquote b ++ unquote a):st
libCat _ = Left "cat requires two parameters"
libCons (a:b:st) = Right $ Quote (b:unquote a):st
libCons _ = Left "cons requires two parameters"

main = do
  let c = Map.fromList [
           ("+", libAdd),
           ("show", libShow),
           ("i", libI c),
           ("zap", libZap),
           ("dup", libDup),
           ("swap", libSwap),
           ("dip", libDip c),
           ("unit", libUnit),
           ("cat", libCat),
           ("cons", libCons)
          ]
  loop c
