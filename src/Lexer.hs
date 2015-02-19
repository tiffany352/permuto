module Lexer (readNumber, readAtom, readString, readClosure, Expr(..), showExprs, readExpr, readSep, readExprs) where

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
      Nothing -> Just (acc, c:s)
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
    else Just ([], c:s)
readAtomTail [] = Just ([], [])

readAtom :: [Char] -> Maybe ([Char], [Char])
readAtom (c:s) =
    if isAlpha c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"
    then fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
    else Just ([], (c:s))
readAtom [] = Nothing

readStringInternal :: [Char] -> [Char] -> Maybe ([Char], [Char])
readStringInternal acc (c:s) =
    case c of
      '"' -> Just (acc, s)
      _ -> readStringInternal (acc ++ [c]) s
readStringInternal acc [] = Nothing

readString :: [Char] -> Maybe ([Char], [Char])
readString (c:s) | c == '"' =
    readStringInternal "" s
readString _ = Nothing

readClosureInternal :: [Expr] -> [Char] -> Maybe ([Expr], [Char])
readClosureInternal es s =
    case readSep s of
      [] -> Nothing
      ']':t -> Just (es, t)
      xs -> readExpr xs >>= \(e, t) -> readClosureInternal (es++[e]) t

readClosure :: [Char] -> Maybe ([Expr], [Char])
readClosure (c:s) | c == '[' =
    readClosureInternal [] s
readClosure _ = Nothing

readQuote :: [Char] -> Maybe (Expr, [Char])
readQuote (c:s) | c == '\'' =
    readExpr s
readQuote _ = Nothing

data Expr = Number Int | Atom [Char] | String [Char] | Quote [Expr]

instance Show Expr where
    show (Number n) = show n
    show (Atom a) = a
    show (String s) = show s
    show (Quote (q:qs)) = (foldl (\a-> \b-> a++" "++(show b)) ("["++show q) qs) ++ "]"
    show (Quote []) = "[]"

showExprs :: [Expr] -> [Char]
showExprs (e:es) = foldl (\a-> \b-> a ++ " " ++ (show b)) (show e) es
showExprs [] = ""

orElse :: Maybe a -> Maybe a -> Maybe a
orElse a b = case a of
               Just x -> Just x
               Nothing -> b

readExpr :: [Char] -> Maybe (Expr, [Char])
readExpr s =
    foldl orElse Nothing [ readQuote s >>= \(v,t) -> Just (Quote [v],t)
                         , readClosure s >>= \(v,t) -> Just (Quote v, t)
                         , readString s >>= \(v,t) -> Just (String v, t)
                         , readNumber s >>= \(v,t) -> Just (Number v, t)
                         , readAtom s >>= \(v,t) -> Just (Atom v, t) ]

readSep :: [Char] -> [Char]
readSep (c:s) | isSpace c = readSep s
readSep s = s

readExprs :: [Char] -> Maybe ([Expr], [Char])
readExprs s =
    readExpr s >>= (\(e,t) ->
      case readSep t of
        [] -> Just ([e],t)
        xs -> fmap (\(es,t) -> (e:es,t)) $ readExprs xs)
