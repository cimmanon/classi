module Types.Color
	( Color(..)
	, hex2Int
	, hslrgb
	, rgbhsl
	, keywords
	) where

import Data.Word (Word8)

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

type Channel = Word8

-- Maybe the color information should be stored as HSL by default instead?
-- there is a big difference between adjusting the hue of RGB black
-- (which will always compute to red) vs HSL black
data Color = Color
	{ red :: Channel
	, blue :: Channel
	, green :: Channel
	, alpha :: Double
	} deriving (Show, Eq)

{-
data RGB = RGB
	{ red :: Channel
	, blue :: Channel
	, green :: Channel
	}

data HSL = HSL
	{ hue :: Int
	, saturation :: Double
	, brightness :: Double
	}
-}

hex2Int :: String -> Word8
hex2Int s = read $ '0' : 'x' : s

{-
Convert a color from HSL to RGB

Formula from the W3C: http://www.w3.org/TR/css3-color/#hsl-color

h = 0-360
s = 0-100
l = 0-100

hslrgb 0 50 50 -- Color 191 64 64 0
hslrgb 0 0 50 -- Color 128 128 128 0
hslrgb 20 15 30 -- Color 88 73 65 0
hslrgb 50 5 50 -- Color 132 134 121 0
hslrgb 50 10 50 -- Color 140 136 115 0
hslrgb 100 50 50 -- Color 106 191 64 0
hslrgb 100 50 75 -- Color 181 223 159 0
hslrgb 100 75 75 -- Color 175 239 143 0
-}
hslrgb :: Int -> Int -> Int -> Color
hslrgb h s l = Color r g b 0
	where
		h' = fromIntegral h / 360
		s' = fromIntegral s / 100
		l' = fromIntegral l / 100
		m1 = l' * 2 - m2
		m2 = if l' <= 0.5
			then l' * (s' + 1)
			else l' + s' - l' * s'
		toRGB x = round $ 255 * hue2rgb m1 m2 x
		r = toRGB $ h' + 1.0 / 3.0
		g = toRGB h'
		b = toRGB $ h' - 1.0 / 3.0

hue2rgb :: Double -> Double -> Double -> Double
hue2rgb m1 m2 h
	| h' * 6 < 1 = m1 + (m2 - m1) * h' * 6
	| h' * 2 < 1 = m2
	| h' * 3 < 2 = m1 + (m2 - m1) * (2 / 3 - h') * 6
	| otherwise = m1
	where
		h'
			| h < 0 = h + 1
			| h > 1 = h - 1
			| otherwise = h

{-
Convert a color from RGB to HSL

Formula: http://en.wikipedia.org/wiki/HSL_and_HSV

rgbhsl 192 64 192 -- (300,50,50)
-}
rgbhsl :: Channel -> Channel -> Channel -> (Int, Int, Int)
rgbhsl r g b = (h, round $ s * 100, round $ l * 100)
	where
		r' = fromIntegral r / 255
		g' = fromIntegral g / 255
		b' = fromIntegral b / 255
		m1 = maximum [r', g', b']
		m2 = minimum [r', g', b']
		c = m1 - m2

		h'
			| m1 == m2 = 0
			| m1 == r' = (g' - b') / c
			| m1 == g' = (b' - r') / c + 2
			| m1 == b' = (r' - g') / c + 4
		h = round h' * 60 `mod` 360
		s
			| l > 0 = c / (1 - abs (2 * l - 1))
			| otherwise = 0
		l = (m1 + m2) / 2

-- TODO: add the rest of the color keywords
keywords :: [(String, Color)]
keywords =
	[ ("white", Color 255 255 255 0)
	, ("black", Color 0 0 0 0)
	]
