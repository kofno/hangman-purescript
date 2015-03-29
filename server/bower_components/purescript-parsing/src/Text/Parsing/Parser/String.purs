module Text.Parsing.Parser.String where

import Data.String
import Data.Either
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Char

import Control.Alt
import Control.Alternative
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State.Class

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

eof :: forall m. (Monad m) => ParserT String m Unit
eof = ParserT $ \s ->
  return $ case s of
    "" -> { consumed: false, input: s, result: Right unit }
    _ -> { consumed: false, input: s, result: Left (strMsg "Expected EOF") }

string :: forall m. (Monad m) => String -> ParserT String m String
string s = ParserT $ \s'  ->
  return $ case indexOf s s' of
    0 -> { consumed: true, input: drop (length s) s', result: Right s }
    _ -> { consumed: false, input: s', result: Left (strMsg ("Expected " ++ show s)) }

char :: forall m. (Monad m) => ParserT String m String
char = ParserT $ \s' ->
  return $ case charAt 0 s' of
    Nothing -> { consumed: false, input: s', result: Left (strMsg "Unexpected EOF") }
    Just c  -> { consumed: true, input: drop 1 s', result: Right (charString c) }

satisfy :: forall m. (Monad m) => (String -> Boolean) -> ParserT String m String
satisfy f = try do
    c <- char
    if f c then return c
           else fail "Character did not satisfy predicate"

whiteSpace :: forall m. (Monad m) => ParserT String m String
whiteSpace = do
  list <- many $ string "\n" <|> string "\r" <|> string " " <|> string "\t"
  return $ foldMap id list

skipSpaces :: forall m. (Monad m) => ParserT String m Unit
skipSpaces = do
  whiteSpace
  return unit

oneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String
oneOf ss = satisfy (flip elem ss)

noneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String
noneOf ss = satisfy (flip notElem ss)
