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

libAdd c (a:b:st) =
    maybeResult "Expected number"
              (Monad.liftM2 (+) (number a) (number b)
               >>= Just . (flip push st) . Number)
libAdd c _ = Left "add requires two parameters"
libShow c (a:st) = Right $ push (String $ show a) st
libShow c _ = Left "show requires a parameter"
libI c (Quote q:st) = fmap (\(st', c)-> st') $ evalExprs c st q
libI c _ = Left "Quoted parameter required"
libZap c (v:st) = Right st
libZap c _ = Left "Can't zap an empty stack"
libDup c (v:st) = Right $ v:v:st
libDup c _ = Left "Can't dup an empty stack"
libSwap c (a:b:st) = Right $ b:a:st
libSwap c _ = Left "swap requires two parameters"
libDip c (Quote q:b:st) = fmap (\(st',c)-> push b st') $ evalExprs c st q
libDip c (Quote q:st) = Left "dip requires two parameters"
libDip c _ = Left "First argument to dip must be quoted"
libUnit c (a:st) = Right $ Quote [a] : st
libUnit c _ = Left "Can't unit empty stack"
libCat c (a:b:st) = Right $ Quote (unquote b ++ unquote a):st
libCat c _ = Left "cat requires two parameters"
libCons c (a:b:st) = Right $ Quote (b:unquote a):st
libCons c _ = Left "cons requires two parameters"

main = do
  let c = Context $ Map.fromList [
           ("+", libAdd),
           ("show", libShow),
           ("i", libI),
           ("zap", libZap),
           ("dup", libDup),
           ("swap", libSwap),
           ("dip", libDip),
           ("unit", libUnit),
           ("cat", libCat),
           ("cons", libCons)
          ]
  loop c
