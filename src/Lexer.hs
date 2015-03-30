module Lexer (Error(..), eof, expected, funcError, Result(..), Context(..), Stack, Expr(..),
              showExprs, maybeResult, readNumber, readAtom, readString, readClosure, readExpr,
              readSep, readExprs) where

import Data.Char
import qualified Data.Map

data Error = EOF | Expected [Char] | UnknownExpression | Func ([Char], [Char]) | User [Char]

instance Show Error where
    show EOF = "End of file"
    show (Expected s) = "Expected one of " ++ s
    show UnknownExpression = "Unknown expression"
    show (Func (f, s)) = "In " ++ f ++ ": " ++ s
    show (User s) = s

eof :: Result a
eof = Left EOF

expected :: [Char] -> Result a
expected s = Left $ Expected s

funcError :: [Char] -> [Char] -> Result a
funcError f s = Left $ Func (f, s)

type Result a = Either Error a

newtype Context = Context { funcs :: Data.Map.Map [Char] (Stack -> Result Stack) }

type Stack = [Expr]

data Expr = Number Int | Atom [Char] | String [Char] | RawQuote [Expr] | Quote (Context, [Expr])

showExprs :: [Expr] -> [Char]
showExprs (e:es) = foldl (\a-> \b-> a ++ " " ++ (show b)) (show e) es
showExprs [] = ""

instance Show Expr where
    show (Number n) = show n
    show (Atom a) = a
    show (String s) = show s
    show (RawQuote [Number n]) = show n
    show (RawQuote [String s]) = show s
    show (RawQuote (q:qs)) = (foldl (\a-> \b-> a++" "++(show b)) ("["++show q) qs) ++ "]"
    show (RawQuote []) = "[]"
    show (Quote (c,e)) = show $ RawQuote e

maybeResult :: Error -> Maybe a -> Result a
maybeResult s (Just x) = Right x
maybeResult s Nothing = Left s

indexOf :: Char -> [Char] -> Maybe Int
indexOf c (x:xs) | c == x = Just 0
indexOf c (x:xs) = fmap ((+)1) $ indexOf c xs
indexOf c [] = Nothing

readNumberContent :: Int -> [Char] -> Result (Int, [Char])
readNumberContent acc (c:s) =
    case indexOf c "0123456789" of
      Just n -> readNumberContent (acc*10+n) s
      Nothing -> Right (acc, c:s)
readNumberContent acc [] = Right (acc, [])

readNumberStart :: [Char] -> Result (Int, [Char])
readNumberStart (c:s) | isDigit c = readNumberContent 0 (c:s)
readNumberStart (c:s) = expected "0-9"
readNumberStart [] = eof

readNumber :: [Char] -> Result (Int, [Char])
readNumber ('-':s) =
    fmap (\(n,t) -> (-n,t)) $ readNumberStart s
readNumber ('+':s) = readNumberStart s
readNumber s = readNumberStart s

oneOf c (x:xs) | c == x = True
oneOf c (x:xs) = oneOf c xs
oneOf c [] = False

isIdent c = isAlpha c || oneOf c "`~!@#$%^&*()-_=+,<.>/?|"

readAtomTail :: [Char] -> Result ([Char], [Char])
readAtomTail (c:s) | isIdent c || isDigit c =
    fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
readAtomTail s = Right ([], s)

readAtom :: [Char] -> Result ([Char], [Char])
readAtom (c:s) | isIdent c =
    fmap (\(xs, t) -> (c:xs, t)) $ readAtomTail s
readAtom (c:s) = expected "~!@#$%^&*()-_=+,<.>/?|"
readAtom [] = eof

readStringInternal :: [Char] -> [Char] -> Result ([Char], [Char])
readStringInternal acc ('"':s) = Right (acc, s)
readStringInternal acc (c:s) = readStringInternal (acc++[c]) s
readStringInternal acc [] = eof

readString :: [Char] -> Result ([Char], [Char])
readString (c:s) | c == '"' = readStringInternal "" s
readString _ = expected "\""

readClosureInternal :: [Expr] -> [Char] -> Result ([Expr], [Char])
readClosureInternal es s =
    case readSep s of
      [] -> eof
      ']':t -> Right (es, t)
      xs -> readExpr xs >>= \(e, t) -> readClosureInternal (es++[e]) t

readClosure :: [Char] -> Result ([Expr], [Char])
readClosure (c:s) | c == '[' = readClosureInternal [] s
readClosure _ = expected "["

readQuote :: [Char] -> Result (Expr, [Char])
readQuote (c:s) | c == '\'' = readExpr s
readQuote _ = expected "'"

orElse :: Result a -> Result a -> Result a
orElse (Right a) _ = Right a
orElse _ (Right a) = Right a
orElse (Left EOF) _ = Left EOF
orElse (Left _) b = b

readExpr :: [Char] -> Result (Expr, [Char])
readExpr s =
    foldl (flip orElse) (Left UnknownExpression)
              [ readQuote s >>= \(v,t) -> Right (RawQuote [v],t)
              , readClosure s >>= \(v,t) -> Right (RawQuote v, t)
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
