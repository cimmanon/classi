module Parsers.Selector
{-	( selector
	)-} where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.List (lookup)
import Data.Monoid ((<>), mconcat, Monoid)
import Text.Parsec
import Text.Parsec.String (Parser)

import Parsers.Common


toList :: a -> [a]
toList = (: [])

escapedLetters :: Parser String
escapedLetters = (:)
	<$> try (char '\\')
	<*> many1 letter

simpleSelectors :: Parser [String]
simpleSelectors = many1 $ elementSelector <|> idSelector <|> classSelector <|> attributeSelector <|> pseudoSelector

elementSelector :: Parser String
elementSelector = mconcat <$> many1 (many1 letter <|> escapedLetters)

classSelector :: Parser String
classSelector = (:) <$> try (char '.') <*> xSelector

idSelector :: Parser String
idSelector = (:) <$> try (char '#') <*> xSelector

xSelector :: Parser String
xSelector = mconcat <$> sel
	where
		sel = (:)
			<$> try ((toList <$> firstCharacter) <|> unicodeCharacter)
			<*> many1 (unicodeCharacter <|> escapedChar <|> many1 (satisfy (`notElem` selectorInvalidCharacters)))
		firstCharacter = try $ satisfy (`elem` validStartCharacters)

pseudoSelector :: Parser String
pseudoSelector = doubleColon <|> singleColon
	where
		singleColon = mconcatP
			[ try $ string ":"
			, function
			]
		doubleColon = mconcatP
			[ try $ string "::"
			, function
			]
		function = mconcatP
			[ try $ many1 (satisfy (`elem` validStartCharacters))
			, args <|> return []
			]
		args = mconcatP
			[ try $ string "("
			, many (satisfy (/= ')'))
			, string ")"
			]

attributeSelector :: Parser String
attributeSelector = mconcatP
	[ try $ string "["
	, spaces *> attributeName <* spaces
	, operator <|> return []
	, try (spaces *> satisfy (`elem` "iI") *> return " i") <|> return []
	, spaces *> string "]"
	]
	where
		attributeName = many1 $ satisfy (`elem` '-' : alphabeticalCharacters)
		operatorCharacters = satisfy (`elem` "~|^$*=")
		operator = mconcatP
			[ try $ many1 operatorCharacters <* spaces
			, quotedString <|> unquotedString
			]

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

{----------------------------------------------------------------------------------------------------{
                                                                      | Helpers
}----------------------------------------------------------------------------------------------------}

alphabeticalCharacters :: [Char]
alphabeticalCharacters = ['a'..'z'] ++ ['A'..'Z']

-- classes and ids must begin with certain characters after their corresponding identifier character
validStartCharacters :: [Char]
validStartCharacters = '-' : '_' : alphabeticalCharacters

selectorInvalidCharacters :: [Char]
selectorInvalidCharacters = "~!@$%^&*()+=,./';:\"?<>[]{}|`# "
