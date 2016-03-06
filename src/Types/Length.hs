module Types.Length
	( Length(..)
	) where

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

{----------------------------------------------------------------------------------------------------{
                                                                      | Type
}----------------------------------------------------------------------------------------------------}

-- TODO: figure out what to do with missing units
data Length = Length
	{ value :: Double
	, unit :: String
	} deriving (Show, Eq)

instance Num Length where
	-- addition
	Length f uf + Length g ug = Length (f + g) uf

	-- multiplication
	Length f uf * Length g ug = Length (f * g) uf

	negate (Length f u) = Length (negate f) u
	abs (Length f u) = Length (abs f) u
	signum (Length f u) = Length (signum f) u
	fromInteger f = Length (fromInteger f) []

instance Fractional Length where
	Length f uf / Length g ug = Length (f / g) uf
	fromRational n = Length (fromRational n) []

{----------------------------------------------------------------------------------------------------{
                                                                      | Functions
}----------------------------------------------------------------------------------------------------}

