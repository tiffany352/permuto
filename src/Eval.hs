module Eval (push, pop, evalExpr, evalExprs, unquote) where

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
                  v -> Just $ push v st

evalExprs :: Context -> Stack -> [Expr] -> Maybe Stack
evalExprs c st (e:es) = evalExpr c st e >>= (\st' -> evalExprs c st' es)
evalExprs c st [] = Just st

unquote :: Expr -> [Expr]
unquote (Quote q) = q
unquote q = [q]
