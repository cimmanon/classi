{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Types

-- why does the return type need to be `Parser[[a]]` rather than `Parser[a]`?
csv :: Parser a -> Parser [[a]]
csv p = many1 p `sepBy1` commaDelimiter

commaDelimiter :: Parser Char
commaDelimiter = spaces *> satisfy (== ',') <* spaces

{----------------------------------------------------------------------------------------------------{
                                                                      | Primitives
}----------------------------------------------------------------------------------------------------}

property :: Parser String
property = many1 (satisfy (\x -> x == '-' || isAlpha x))
	<?> "CSS property (eg. `display` or `-moz-border-radius`)"


percentage :: Parser Length
percentage = Length
	<$> do read <$> many1 digit
	<*> string "%"
	<?> "percentage"

--value :: Parser [Value]

color :: Parser Color
color =
	hexColor <|> rgbColor
	where
		hexColor = do
			satisfy (== '#')
			x <- many1 hexDigit
			case x of
				r1:r2:g1:g2:b1:b2:[] -> return $ Color (hex2Int [r1, r2]) (hex2Int [g1, g2]) (hex2Int [b1, b2]) 0
				r:g:b:[] -> return $ Color (hex2Int [r, r]) (hex2Int [g, g]) (hex2Int [b, b]) 0
				_ -> unexpected "Invalid format for hexadecimal color (eg. #333 or #333333)"
		rgbColor = do
			Text.Parsec.string "rgb("
			spaces
			colors <- csv digit
			spaces
			satisfy (== ')')
			case colors of
				r:g:b:[] -> return $ Color (read r) (read g) (read b) 0
				_ -> unexpected $ "Expected 3 colors values for rgb format, received " <> (show $ length colors)
		{-
		rgbaColor = do
			string "rgba(" <* spaces
			colors <- csv digit
			satisfy (== ')')
			case colors of
				r:g:b:a:[] -> return $ Color (read r) (read g) (read b) 0
				_ -> unexpected "Invalid format for rgba color (eg. rgb(255, 255, 255, 0.5))"
		-}
		hslColor = do
			Text.Parsec.string "hsl("
			spaces
			h <- many1 digit
			s <- value <$> do commaDelimiter *> percentage
			l <- value <$> do commaDelimiter *> percentage
			satisfy (== ')')


{----------------------------------------------------------------------------------------------------{
                                                                      | Selectors
}----------------------------------------------------------------------------------------------------}

{-
Selectors:

* element: body, p
* class: .foo
* id: #bar
* descendant: body p
* direct descendant: body > p
* adjacent sibling: p + p
* sibling: p ~ p
* attribute: input[type], input[type="text"]
* pseudoclass: p:last-child, p:nth(2n+1), p:last-of-type, input:focus, p:first-letter, div:empty
-}
--selector :: Parser ByteString

{----------------------------------------------------------------------------------------------------{
                                                                      | Media Queries
}----------------------------------------------------------------------------------------------------}

{-
@media not screen and (min-width: 30em), (max-width: 30em) {
	.foo {
		color: red;
	}
}

Rules:

* Comma is used to delimit individual queries (your `or` operator)
* The `and` keyword must be used to join query information together
* The `not` keyword can only be used at the beginning and negates the entire query
* Parentheses must surround comparitive values like min-width, pixel-density, etc.
-}

{----------------------------------------------------------------------------------------------------{
                                                                      | Feature Queries
}----------------------------------------------------------------------------------------------------}

{-
http://www.w3.org/TR/css3-conditional/#at-supports

@supports ((display: flex) or (display: -webkit-flex) or (display: -ms-flexbox)) and (background: red) {
	.foo {
		color: red
	}
}

Rules:

* All property/value pairs must be enclosed in parentheses, the value information cannot be omitted
* The `and` and `or` operators cannot be mixed, you must use parentheses to remove ambiguity (per the spec)
-}