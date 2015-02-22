module Eval (push, pop, Context(..), Stack, evalExpr, evalExprs, unquote, number) where

import Data.Map as Map
import Data.Maybe
import Lexer

push v st = v:st

pop (v:st) = (v, st)

newtype Context = Context { funcs :: Map [Char] (Context -> Stack -> Result Stack) }

type Stack = [Expr]

wrapFunc v c s = fmap (\(s',_) -> s') $ evalExprs c s v

libDef :: Context -> Stack -> Result (Stack, Context)
libDef c (n:Quote v:st) =
    case string n of
      Just n' ->
          Right (st, Context $ Map.insert n'
                 (wrapFunc v) (funcs c))
      Nothing -> Left "Right parameter of def must be string"
libDef c (n:v:st) = Left "Left parameter of def must be quoted"
libDef c _ = Left "def requires two parameters"

evalExpr :: Context -> Stack -> Expr -> Result (Stack, Context)
evalExpr c st (Atom "def") = libDef c st
evalExpr c st (Atom a) =
    case Map.lookup a (funcs c) of
      Just x -> fmap (\st' -> (st', c)) $ x c st
      Nothing -> Left $ "No such atom '"++a++"'"
evalExpr c st (Quote q) =
    Right (push (Quote q) st, c)
evalExpr c st v =
    Right (push (Quote [v]) st, c)

evalExprs :: Context -> Stack -> [Expr] -> Result (Stack, Context)
evalExprs c st (e:es) = evalExpr c st e >>= (\(st',c') -> evalExprs c' st' es)
evalExprs c st [] = Right (st, c)

unquote :: Expr -> [Expr]
unquote (Quote q) = q
unquote q = [q]

number :: Expr -> Maybe Int
number (Quote [Number e]) = Just e
number (Number e) = Just e
number _ = Nothing

string :: Expr -> Maybe [Char]
string (Quote [String s]) = Just s
string (String s) = Just s
string _ = Nothing
