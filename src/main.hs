import Lexer
import Eval
import Data.Map as Map
import Data.Maybe
import Control.Monad as Monad
import qualified System.Environment
import qualified GHC.IO.Handle
import qualified GHC.IO.Handle.FD

read :: [Char] -> Result [Expr]
read s = case readExprs s of
           Right (es, []) -> Right es
           Right (_, s) -> Left $ User $ "Junk data after expression: " ++ s
           Left x -> Left x

putStrLnNotEmpty [] = return ()
putStrLnNotEmpty s = putStrLn s

hasBlock (c:s) | c == ':' = readSep s == [] || hasBlock s
hasBlock (c:s) = hasBlock s
hasBlock [] = False

indent n = "\n" ++ replicate (n*2) ' '

loop c acc i = do
  putStr $ replicate (i+1) '>' ++ " "
  GHC.IO.Handle.hFlush GHC.IO.Handle.FD.stdout
  line <- getLine
  let acc' = if acc == []
             then line
             else acc ++ indent i ++ line
  let r = Main.read acc' >>= evalExprs c []
  case (line, r) of
    ("quit", _) -> return ()
    ([], _) -> loop c acc (max 0 $ i-1)
    (x, _) | hasBlock x -> loop c acc' (i+1)
    (_, _) | i > 0 -> loop c acc' i
    (_, Right (x, c')) -> do
                   putStrLnNotEmpty $ Lexer.showExprs x
                   loop c' [] 0
    (_, Left EOF) -> putStr "EOF" >> loop c acc' (i+1)
    (_, Left x) -> do
                   putStrLnNotEmpty $ show x
                   loop c [] 0

runFile c f = do
  contents <- readFile f
  let r = Main.read contents >>= evalExprs c []
  putStrLn $ case r of
    Right (x, _) -> Lexer.showExprs x
    Left x -> show x

libAdd (a:b:st) =
    maybeResult (User "Expected number")
              (Monad.liftM2 (+) (number a) (number b)
               >>= Just . (flip push st) . Number)
libAdd _ = funcError "add" "Requires two parameters"
libShow (a:st) = Right $ push (String $ show a) st
libShow _ = funcError "show" "Requires a parameter"
libI (Quote (c,q):st) = fmap (\(st', c)-> st') $ evalExprs c st q
libI (RawQuote q:st) = Left $ User "Quote does not have context?"
libI _ = funcError "i" "Parameter must be quote"
libZap (v:st) = Right st
libZap _ = funcError "zap" "Empty stack"
libDup (v:st) = Right $ v:v:st
libDup _ = funcError "dup" "Empty stack"
libSwap (a:b:st) = Right $ b:a:st
libSwap _ = funcError "swap" "Requires two parameters"
libDip (Quote (c,q):b:st) = fmap (\(st',c)-> push b st') $ evalExprs c st q
libDip (RawQuote q:b:st) = Left $ User "Quote does not have context?"
libDip (Quote q:st) = funcError "dip" "Requires two parameters"
libDip _ = funcError "dip" "Parameter 1 must be quote"

main = do
  args <- System.Environment.getArgs
  let c = Context $ Map.fromList [
           ("+", libAdd),
           ("show", libShow),
           ("i", libI),
           ("zap", libZap),
           ("dup", libDup),
           ("swap", libSwap),
           ("dip", libDip)
          ]
  if Prelude.null args
  then loop c [] 0
  else Prelude.foldl (>>) (return ()) $ fmap (runFile c) args
