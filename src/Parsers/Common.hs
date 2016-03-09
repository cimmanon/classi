module Parsers.Common where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Types

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
	<$> do read <$> many1 digit
	<*> string "%"
	<?> "percentage"

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
