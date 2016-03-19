module Parsers.Common where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.Monoid ((<>), mappend, Monoid, mconcat)
import Text.Parsec
import Text.Parsec.String (Parser)

import Types

-- shortcut operator for mappending the results of parsers
(<++>) :: (Monoid a) => Parser a -> Parser a -> Parser a
x <++> y = mappend <$> x <*> y

decimal :: Parser String
decimal = do
	w <- many digit
	satisfy (== '.')
	f <- many1 digit
	return $ w <> "." <> f

-- why does the return type need to be `Parser[[a]]` rather than `Parser[a]`?
csv :: Parser a -> Parser [[a]]
csv p = many1 p `sepBy1` commaDelimiter

commaDelimiter :: Parser Char
commaDelimiter = spaces *> satisfy (== ',') <* spaces

percentage :: Parser Dimension
percentage = Dimension
	<$> do read <$> (try decimal <|> many1 digit)
	<*> string "%"
	<?> "percentage"

escapedChar :: Parser String
escapedChar = (:)
	<$> try (char '\\')
	<*> ((:) <$> anyChar <*> return [])

mconcatP :: Monoid a => [Parser a] -> Parser a
mconcatP = fmap mconcat . sequence
{-
not quite sure exactly what characters we'll want to use for end of token delimiters
absolutely certain on the following:
	,
	\n
	\t
	\r
	space
	;

maybe tokens:
	(
	)
	/
-}
isEndOfToken :: Char -> Bool
isEndOfToken = (`elem` " ,\n\t\r;()")

quotedString :: Parser String
quotedString = mconcatP
	[ try $ string "\""
	, mconcat <$> many (many1 (satisfy (`notElem` "\"")) <|> escapedChar)
	, string "\""
	]

unquotedString :: Parser String
unquotedString = mconcat <$> try (many1 $ many1 (try $ satisfy (`notElem` "\" \\[]")) <|> escapedChar)
