module Eval (push, pop, Context, Stack, evalExpr, evalExprs, unquote, number) where

import Data.Map as Map
import Data.Maybe
import Lexer

push v st = v:st

pop (v:st) = (v, st)

type Context = Map [Char] (Stack -> Result Stack)

type Stack = [Expr]

evalExpr :: Context -> Stack -> Expr -> Result Stack
evalExpr c st e = case e of
                  Atom a -> case Map.lookup a c of
                              Just x -> x st
                              Nothing -> Left $ "No such atom '"++a++"'"
                  Quote q -> Right $ push (Quote q) st
                  v -> Right $ push (Quote [v]) st

evalExprs :: Context -> Stack -> [Expr] -> Result Stack
evalExprs c st (e:es) = evalExpr c st e >>= (\st' -> evalExprs c st' es)
evalExprs c st [] = Right st

unquote :: Expr -> [Expr]
unquote (Quote q) = q
unquote q = [q]

number :: Expr -> Maybe Int
number (Quote [Number e]) = Just e
number (Number e) = Just e
number _ = Nothing
