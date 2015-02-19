module Eval (push, pop, evalExpr, evalExprs) where

import Data.Map as Map
import Data.Maybe
import Lexer

push v st = v:st

pop (v:st) = (v, st)

type Context = Map [Char] (Stack -> Stack)

type Stack = [Expr]

evalExpr :: Context -> Stack -> Expr -> Maybe Stack
evalExpr c st e = case e of
                  Number n -> Just $ push (Number n) st
                  Atom a -> fmap (\x -> x st) $ Map.lookup a c

evalExprs :: Context -> Stack -> [Expr] -> Maybe Stack
evalExprs c st (e:es) = evalExpr c st e >>= (\st' -> evalExprs c st' es)
evalExprs c st [] = Just st
