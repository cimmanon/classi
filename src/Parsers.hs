{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Data.Char
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Types
import Parsers.Color

{----------------------------------------------------------------------------------------------------{
                                                                      | Primitives
}----------------------------------------------------------------------------------------------------}

property :: Parser String
property = many1 (satisfy (\x -> x == '-' || isAlpha x))
	<?> "CSS property (eg. `display` or `-moz-border-radius`)"


--value :: Parser [Value]

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