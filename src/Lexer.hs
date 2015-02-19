module Lexer (readNumber, readAtom, Expr(..), readExpr, readSep, readExprs) where

import Data.Char

indexOf :: Char -> [Char] -> Maybe Int
indexOf c (x:xs) =
    if c == x
    then Just 0
    else fmap ((+)1) $ indexOf c xs
indexOf c [] = Nothing

readNumberContent :: Int -> [Char] -> Maybe (Int, [Char])
readNumberContent acc (c:s) =
    case indexOf c "0123456789" of
      Just n -> readNumberContent (acc*10+n) s
      Nothing -> Just (acc, s)
readNumberContent acc [] = Just (acc, [])

readNumberStart :: [Char] -> Maybe (Int, [Char])
readNumberStart (c:s) =
    if isDigit c
    then readNumberContent 0 (c:s)
    else Nothing
readNumberStart [] = Nothing

readNumber :: [Char] -> Maybe (Int, [Char])
readNumber (c:s) =
    case c of
      '-' -> fmap (\(n,t) -> (-n,t)) $ readNumberStart s
      '+' -> readNumberStart s
      _ -> readNumberStart (c:s)
readNumber [] = Nothing

oneOf c (x:xs) =
    if c == x
    then True
    else oneOf c xs
oneOf c [] =
    False

readAtomTail :: [Char] -> Maybe ([Char], [Char])
readAtomTail (c:s) =
    if isAlphaNum c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"
    then fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
    else Just ([], s)
readAtomTail [] = Just ([], [])

readAtom :: [Char] -> Maybe ([Char], [Char])
readAtom (c:s) =
    if isAlpha c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"
    then fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
    else Just ([], (c:s))
readAtom [] = Nothing

data Expr = Number Int | Atom [Char]

instance Show Expr where
    show (Number n) = "Number " ++ show n
    show (Atom a) = "Atom " ++ show a

readExpr :: [Char] -> Maybe (Expr, [Char])
readExpr s =
    case readNumber s of
      Nothing -> fmap (\(s,t) -> (Atom s, t)) $ readAtom s
      Just (n, t) -> Just (Number n, t)

readSep :: [Char] -> [Char]
readSep (c:s) =
    if isSpace c
    then readSep s
    else (c:s)
readSep [] = []

readExprs :: [Char] -> Maybe ([Expr], [Char])
readExprs s =
    readExpr s >>= (\(e,t) ->
      case readSep t of
        [] -> Just ([e],t)
        xs -> fmap (\(es,t) -> (e:es,t)) $ readExprs xs)
