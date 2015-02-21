module Lexer (Result(..), maybeResult, readNumber, readAtom, readString, readClosure, Expr(..), showExprs, readExpr, readSep, readExprs) where

import Data.Char

type Result a = Either [Char] a

maybeResult :: [Char] -> Maybe a -> Result a
maybeResult s (Just x) = Right x
maybeResult s Nothing = Left s

eof :: Result a
eof = Left "Unexpected EOF"

indexOf :: Char -> [Char] -> Maybe Int
indexOf c (x:xs) =
    if c == x
    then Just 0
    else fmap ((+)1) $ indexOf c xs
indexOf c [] = Nothing

readNumberContent :: Int -> [Char] -> Result (Int, [Char])
readNumberContent acc (c:s) =
    case indexOf c "0123456789" of
      Just n -> readNumberContent (acc*10+n) s
      Nothing -> Right (acc, c:s)
readNumberContent acc [] = Right (acc, [])

readNumberStart :: [Char] -> Result (Int, [Char])
readNumberStart (c:s) =
    if isDigit c
    then readNumberContent 0 (c:s)
    else Left "Expected digit"
readNumberStart [] = eof

readNumber :: [Char] -> Result (Int, [Char])
readNumber (c:s) =
    case c of
      '-' -> fmap (\(n,t) -> (-n,t)) $ readNumberStart s
      '+' -> readNumberStart s
      _ -> readNumberStart (c:s)
readNumber [] = eof

oneOf c (x:xs) =
    if c == x
    then True
    else oneOf c xs
oneOf c [] =
    False

readAtomTail :: [Char] -> Result ([Char], [Char])
readAtomTail (c:s) =
    if isAlphaNum c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"
    then fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
    else Right ([], c:s)
readAtomTail [] = Right ([], [])

readAtom :: [Char] -> Result ([Char], [Char])
readAtom (c:s) =
    if isAlpha c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"
    then fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
    else Left "Expected identifier start"
readAtom [] = eof

readStringInternal :: [Char] -> [Char] -> Result ([Char], [Char])
readStringInternal acc (c:s) =
    case c of
      '"' -> Right (acc, s)
      _ -> readStringInternal (acc ++ [c]) s
readStringInternal acc [] = eof

readString :: [Char] -> Result ([Char], [Char])
readString (c:s) | c == '"' =
    readStringInternal "" s
readString _ = Left "Expected string start"

readClosureInternal :: [Expr] -> [Char] -> Result ([Expr], [Char])
readClosureInternal es s =
    case readSep s of
      [] -> eof
      ']':t -> Right (es, t)
      xs -> readExpr xs >>= \(e, t) -> readClosureInternal (es++[e]) t

readClosure :: [Char] -> Result ([Expr], [Char])
readClosure (c:s) | c == '[' =
    readClosureInternal [] s
readClosure _ = Left "Expected ["

readQuote :: [Char] -> Result (Expr, [Char])
readQuote (c:s) | c == '\'' =
    readExpr s
readQuote _ = Left "Expected '"

data Expr = Number Int | Atom [Char] | String [Char] | Quote [Expr]

instance Show Expr where
    show (Number n) = show n
    show (Atom a) = a
    show (String s) = show s
    show (Quote [Number n]) = show n
    show (Quote [String s]) = show s
    show (Quote (q:qs)) = (foldl (\a-> \b-> a++" "++(show b)) ("["++show q) qs) ++ "]"
    show (Quote []) = "[]"

showExprs :: [Expr] -> [Char]
showExprs (e:es) = foldl (\a-> \b-> a ++ " " ++ (show b)) (show e) es
showExprs [] = ""

orElse :: Result a -> Result a -> Result a
orElse a b = case a of
               Right x -> Right x
               Left _ -> b

readExpr :: [Char] -> Result (Expr, [Char])
readExpr s =
    foldl (flip orElse) (Left "No matched rule")
              [ readQuote s >>= \(v,t) -> Right (Quote [v],t)
              , readClosure s >>= \(v,t) -> Right (Quote v, t)
              , readString s >>= \(v,t) -> Right (String v, t)
              , readNumber s >>= \(v,t) -> Right (Number v, t)
              , readAtom s >>= \(v,t) -> Right (Atom v, t) ]

readSep :: [Char] -> [Char]
readSep (c:s) | isSpace c = readSep s
readSep s = s

readExprs :: [Char] -> Result ([Expr], [Char])
readExprs s =
    readExpr s >>= (\(e,t) ->
      case readSep t of
        [] -> Right ([e],t)
        xs -> fmap (\(es,t) -> (e:es,t)) $ readExprs xs)
