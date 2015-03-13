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

percentage :: Parser Length
percentage = Length
	<$> do read <$> many1 digit
	<*> string "%"
	<?> "percentage"
