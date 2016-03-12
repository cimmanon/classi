module Parsers.Color
	( color
	) where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.List (lookup)
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Types
import Types.Color
import Parsers.Common

color :: Parser Color
color =
	(try hexColor) <|> (try rgbColor) <|> (try rgbaColor) <|> (try hslColor) <|> (try hslaColor) <|> (try keyword)

hexColor :: Parser Color
hexColor = do
	satisfy (== '#')
	x <- many1 hexDigit
	case x of
		r1:r2:g1:g2:b1:b2:[] -> return $ Color (hex2Int [r1, r2]) (hex2Int [g1, g2]) (hex2Int [b1, b2]) 0
		r:g:b:[] -> return $ Color (hex2Int [r, r]) (hex2Int [g, g]) (hex2Int [b, b]) 0
		_ -> unexpected "Invalid format for hexadecimal color (eg. #333 or #333333)"

rgbColor :: Parser Color
rgbColor = do
	try $ Text.Parsec.string "rgb("
	spaces
	r <- read <$> many1 digit
	g <- read <$> do commaDelimiter *> many1 digit
	b <- read <$> do commaDelimiter *> many1 digit
	spaces *> satisfy (== ')')
	return $ Color r g b 0

rgbaColor :: Parser Color
rgbaColor = do
	try $ Text.Parsec.string "rgba("
	spaces
	r <- read <$> many1 digit
	g <- read <$> do commaDelimiter *> many1 digit
	b <- read <$> do commaDelimiter *> many1 digit
	a <- do commaDelimiter *> decimal
	spaces *> satisfy (== ')')
	return $ Color r g b (read a)

hslColor :: Parser Color
hslColor = do
	Text.Parsec.string "hsl("
	spaces
	h <- read <$> many1 digit
	s <- value <$> do commaDelimiter *> percentage
	l <- value <$> do commaDelimiter *> percentage
	spaces *> satisfy (== ')')
	return $ hslrgb h s l

hslaColor :: Parser Color
hslaColor = do
	Text.Parsec.string "hsla("
	spaces
	h <- read <$> many1 digit
	s <- value <$> do commaDelimiter *> percentage
	l <- value <$> do commaDelimiter *> percentage
	a <- read <$> do commaDelimiter *> decimal
	spaces *> satisfy (== ')')
	return $ (hslrgb h s l) { alpha = a }

keyword :: Parser Color
keyword = try $ do
	n <- map toLower <$> many1 letter
	case n `lookup` keywords of
		Just c -> return c
		Nothing -> parserZero