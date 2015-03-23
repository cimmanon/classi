{-# LANGUAGE OverloadedStrings, GADTs #-}

module Types
	( module Types.Color
--	, ListDelimiter(..)
	, Length(..)
	) where

import Types.Color

-- Going to want to have a mapping type here as well, but that can come later
--data Value = Color | Length | Int | CString | CList

data ListDelimiter = Comma | Space

data CssType a where
	CssString :: String -> CssType String
	CssColor :: Color -> CssType Color
	CssLength :: Length -> CssType Length
	CssList :: ([CssType a], ListDelimiter) -> CssType ([CssType a], ListDelimiter)
	CssMapping :: [(String, CssType a)] -> CssType [(String, CssType a)]
	-- ^ the "keys" for mappings need to be restricted to String to avoid confusion when using Color names (eg. red)

-- we have to make our own instances for GADTs, we can't just derive it like normal
-- TODO: make nicer show instances
instance (Show a) => Show (CssType a) where
	show (CssString s) = show s
	show (CssColor c) = show c
	show (CssLength l) = show l
	show (CssList xs) = show xs
	show (CssMapping xs) = show xs

{-
lighten :: CssType Color -> CssType Color
lighten = id

lighten' :: CssType Color -> Color
lighten' (CssColor c) = c
-}

{----------------------------------------------------------------------------------------------------{
                                                                      | Color
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                      | Length
}----------------------------------------------------------------------------------------------------}

{-
https://developer.mozilla.org/en-US/docs/Web/CSS/length
https://developer.mozilla.org/en-US/docs/Web/CSS/percentage
https://developer.mozilla.org/en-US/docs/Web/CSS/angle
https://developer.mozilla.org/en-US/docs/Web/CSS/timing-function
https://developer.mozilla.org/en-US/docs/Web/CSS/time

A length is an algebraic data type consisting of a number value and a unit

* 12px
* 1.1em

Some values can be converted from one to another, which would allow arithmetic operations between them:

* cm <-> in <-> mm
* pt <-> pc
* deg <-> grad <-> turn <-> rad
* s <-> ms

It should be possible to perform certain types of arithmetic:

* `100px * 2` is equal to 200px
* `100px / 1px` is equal to 100
* `100 * 1px` is equal to 100px
* `100px + 100px` is equal to 200px

Which unit should be displayed when performing arithmetic between compatible unit types?
Does `1in + 2.54cm` output as 2in or 5.08cm?
-}

data Length = Length
	{ value :: Double
	, unit :: String
	} deriving (Show, Eq)
