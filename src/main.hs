import Lexer
import Eval
import Data.Map as Map
import Data.Maybe
import Control.Monad as Monad

read :: [Char] -> Result [Expr]
read s = case readExprs s of
           Right (es, []) -> Right es
           Left x -> Left x

print :: Result ([Expr], Context) -> [Char]
print (Right (st,_)) = Lexer.showExprs . reverse $ st
print (Left e) = e

once c s = Main.print $ Main.read s >>= evalExprs c []

loop c = do
  line <- getLine
  let r = Main.read line >>= evalExprs c []
  putStrLn $ Main.print r
  case r of
    Right (_, c') -> loop c'
    _ -> loop c

libAdd (a:b:st) =
    maybeResult "Expected number"
              (Monad.liftM2 (+) (number a) (number b)
               >>= Just . (flip push st) . Number)
libAdd _ = Left "add requires two parameters"
libShow (a:st) = Right $ push (String $ show a) st
libShow _ = Left "show requires a parameter"
libI (Quote (c,q):st) = fmap (\(st', c)-> st') $ evalExprs c st q
libI (RawQuote q:st) = Left "Quote does not have context?"
libI _ = Left "Quoted parameter required"
libZap (v:st) = Right st
libZap _ = Left "Can't zap an empty stack"
libDup (v:st) = Right $ v:v:st
libDup _ = Left "Can't dup an empty stack"
libSwap (a:b:st) = Right $ b:a:st
libSwap _ = Left "swap requires two parameters"
libDip (Quote (c,q):b:st) = fmap (\(st',c)-> push b st') $ evalExprs c st q
libDip (RawQuote q:b:st) = Left "Quote does not have context?"
libDip (Quote q:st) = Left "dip requires two parameters"
libDip _ = Left "First argument to dip must be quoted"

main = do
  let c = Context $ Map.fromList [
           ("+", libAdd),
           ("show", libShow),
           ("i", libI),
           ("zap", libZap),
           ("dup", libDup),
           ("swap", libSwap),
           ("dip", libDip)
          ]
  loop c
