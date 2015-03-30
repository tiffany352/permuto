module Eval (push, pop, Context(..), Stack, evalExpr, evalExprs, unquote, number) where

import Data.Map as Map
import Data.Maybe
import Lexer

push v st = v:st

pop (v:st) = (v, st)

wrapFunc v c s = fmap (\(s',_) -> s') $ evalExprs c s v

libDef :: Stack -> Result (Stack, Context)
libDef (n:Quote (c,v):st) =
    case n of
      Quote (_, [Atom n']) ->
          Right (st, Context $ Map.insert n'
                 (wrapFunc v c) (funcs c))
      _ -> funcError "def" "Parameter 1 must be quoted atom"
libDef (n:v:st) = funcError "def" "Parameter 2 must be quoted"
libDef _ = funcError "def" "Requires two parameters"
libUnit c (a:st) = Right (Quote (c,[a]) : st, c)
libUnit c _ = funcError "unit" "Empty stack"
libCat c (a:b:st) = Right (Quote (c, unquote b ++ unquote a):st, c)
libCat c _ = funcError "cat" "Requires two parameters"
libCons c (a:b:st) = Right (Quote (c, b:unquote a):st, c)
libCons c _ = funcError "cons" "Requires two parameters"

evalExpr :: Context -> Stack -> Expr -> Result (Stack, Context)
evalExpr c st (Atom "def") = libDef st
evalExpr c st (Atom "unit") = libUnit c st
evalExpr c st (Atom "cat") = libCat c st
evalExpr c st (Atom "cons") = libCons c st
evalExpr c st (Atom a) =
    case Map.lookup a (funcs c) of
      Just x -> fmap (\st' -> (st', c)) $ x st
      Nothing -> Left $ User $ "No such atom '"++a++"'"
evalExpr c st (RawQuote q) =
    Right (push (Quote (c, q)) st, c)
evalExpr c st (Quote (c',q)) =
    Right (push (Quote (c',q)) st, c)
evalExpr c st v =
    Right (push (Quote (c,[v])) st, c)

evalExprs :: Context -> Stack -> [Expr] -> Result (Stack, Context)
evalExprs c st (e:es) = evalExpr c st e >>= (\(st',c') -> evalExprs c' st' es)
evalExprs c st [] = Right (st, c)

unquote :: Expr -> [Expr]
unquote (Quote (c,q)) = q
unquote (RawQuote q) = q
unquote q = [q]

number :: Expr -> Maybe Int
number (Quote (c,[Number e])) = Just e
number (RawQuote [Number e]) = Just e
number (Number e) = Just e
number _ = Nothing

string :: Expr -> Maybe [Char]
string (Quote (c,[String s])) = Just s
string (RawQuote [String s]) = Just s
string (String s) = Just s
string _ = Nothing
