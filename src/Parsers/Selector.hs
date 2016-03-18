module Parsers.Selector
{-	( selector
	)-} where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.List (lookup)
import Data.Monoid ((<>), mconcat)
import Text.Parsec
import Text.Parsec.String (Parser)

import Parsers.Common

-- should this go into Common?
escapedChar :: Parser String
escapedChar = (:)
	<$> try (char '\\')
	<*> ((:) <$> anyChar <*> return [])

toList :: a -> [a]
toList x = x : []

escapedLetters :: Parser String
escapedLetters = (:)
	<$> try (char '\\')
	<*> many1 letter

elementSelector :: Parser String
elementSelector = mconcat <$> many1 (many1 letter <|> escapedLetters)

classSelector :: Parser String
classSelector = (:) <$> try (char '.') <*> xSelector

xSelector :: Parser String
xSelector = mconcat <$> sel
	where
		sel = (:)
			<$> try (firstCharacter <|> unicodeCharacter)
			<*> many1 (unicodeCharacter <|> escapedChar <|> many1 (satisfy (`notElem` selectorInvalidCharacters)))
		firstCharacter = many1 $ satisfy (`elem` validStartCharacters)

-- https://www.w3.org/International/questions/qa-escapes
-- this is an escape character, followed by N amount of hexadecimal characters, followed by an optional space
unicodeCharacter :: Parser String
unicodeCharacter = (++)
	<$> character
	<*> optionalSingleSpace
	where
		character = (:)
			<$> try (char '\\')
			<*> many1 hexDigit
		optionalSingleSpace = try $
			(toList <$> char ' ') <|> return []

-- classes and ids must begin with certain characters after their corresponding identifier character
validStartCharacters :: [Char]
validStartCharacters = '-' : '_' : ['a'..'z'] ++ ['A'..'Z']

selectorInvalidCharacters :: [Char]
selectorInvalidCharacters = "~!@$%^&*()+=,./';:\"?<>[]{}|`# "
