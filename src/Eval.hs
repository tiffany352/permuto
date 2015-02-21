module Eval (push, pop, evalExpr, evalExprs, unquote, number) where

import Data.Map as Map
import Data.Maybe
import Lexer

push v st = v:st

pop (v:st) = (v, st)

type Context = Map [Char] (Stack -> Maybe Stack)

type Stack = [Expr]

evalExpr :: Context -> Stack -> Expr -> Maybe Stack
evalExpr c st e = case e of
                  Atom a -> Map.lookup a c >>= (\x -> x st)
                  Quote q -> Just $ push (Quote q) st
                  v -> Just $ push (Quote [v]) st

evalExprs :: Context -> Stack -> [Expr] -> Maybe Stack
evalExprs c st (e:es) = evalExpr c st e >>= (\st' -> evalExprs c st' es)
evalExprs c st [] = Just st

unquote :: Expr -> [Expr]
unquote (Quote q) = q
unquote q = [q]

number :: Expr -> Maybe Int
number (Quote [Number e]) = Just e
number (Number e) = Just e
number _ = Nothing
