{-# LANGUAGE OverloadedStrings #-}

module Types where

-- Going to want to have a mapping type here as well, but that can come later
--data Value = Color | Length | Int | CString | CList

data ListDelimiter = Comma | Space

data CString = CString
	{ string :: String
	, quote :: Maybe Char
	}
{-
data CList = CList
	{ values :: [Value]
	, delimiter :: ListDelimiter
	}
-}

{----------------------------------------------------------------------------------------------------{
                                                                      | Color
}----------------------------------------------------------------------------------------------------}

{-
https://developer.mozilla.org/en-US/docs/Web/CSS/color_value

Color needs to contain the following information:

* red (0-255 integer)
* blue (0-255 integer)
* green (0-255 integer)
* alpha (0-1 double)

It needs to have read(?) instances for the following formats:

* #333
* #121212
* rgb(255, 255, 255)
* rgba(255, 255, 255, 0.5)
* hsl(120, 100%, 25%)
* hsla(240, 100%, 50%, 0.7)
* white, goldenrod
* transparent (the specification states that transparent is equal to rgba(0, 0, 0, 0))
-}

data Color = Color
	{ red :: Int
	, blue :: Int
	, green :: Int
	, alpha :: Double
	} deriving (Show, Eq)

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
