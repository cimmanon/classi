module Types.Color
{-	( Color(..)
	, hex2Int
	, hslrgb
	, rgbhsl
	, lighten
	, keywords
	)-} where

import Data.Word (Word8)
import Control.Arrow (second)

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
	, green :: Channel
	, blue :: Channel
	, alpha :: Double
	} deriving (Show, Eq)

rgbToColor :: (Channel, Channel, Channel) -> Color
rgbToColor (r, g, b) = Color r g b 1

{-
data RGB = RGB
	{ red :: Channel
	, blue :: Channel
	, green :: Channel
	}

data HSL = HSL
	{ hue :: Int
	, saturation :: Double
	, lightness :: Double
	}
-}

hex2Int :: String -> Word8
hex2Int s = read $ '0' : 'x' : s

{----------------------------------------------------------------------------------------------------{
                                                                      | HSL to RGB
}----------------------------------------------------------------------------------------------------}

{-
Formula from the W3C: http://www.w3.org/TR/css3-color/#hsl-color

h = 0.0-360.0
s = 0.0-100.0
l = 0.0-100.0
-}
hslrgb :: (Integral a, RealFrac b) => (a, b, b) -> (Channel, Channel, Channel)
hslrgb (h, s, l) = (r, g, b)
	where
		-- normalize h, s, l to be fractions 0..1
		h' = fromIntegral h / 360
		s' = s / 100
		l' = l / 100

		m1 = l' * 2 - m2
		m2 = if l' <= 0.5
			then l' * (s' + 1)
			else l' + s' - l' * s'
		toChannel x = properRound $ roundPlaces 8 $ 255 * hue2rgb m1 m2 x
		r = toChannel $ h' + 1.0 / 3.0
		g = toChannel h'
		b = toChannel $ h' - 1.0 / 3.0

hue2rgb :: (RealFrac a) => a -> a -> a -> a
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

-- since `round` rounds down on 2.5, we need to write a proper rounding function that will round up
properRound :: (RealFrac a, Integral b) => a -> b
properRound x =
	let
		(whole, frac) = properFraction x
	in
		if frac >= 0.5
			then whole + 1
			else whole

-- some of our rounding needs to be a bit fuzzy to guarantee we get the right values
-- in particular, hslrgb conversion for Olive, Purple, and Teal come out to be off-by-one without it
roundPlaces :: (Integral a, RealFrac b) => a -> b -> b
roundPlaces places n =
	let
		multiplier = fromIntegral $ 10 ^ places
		rounded = fromIntegral $ properRound $ n * multiplier
	in
		rounded / multiplier

{----------------------------------------------------------------------------------------------------{
                                                                      | RGB to HSL
}----------------------------------------------------------------------------------------------------}

-- Formula: http://en.wikipedia.org/wiki/HSL_and_HSV

rgbhsl :: (Integral a, RealFrac b) => (Channel, Channel, Channel) -> (a, b, b)
rgbhsl (r, g, b) = (h, s * 100, l * 100)
	where
		r' = fromIntegral r / 255
		g' = fromIntegral g / 255
		b' = fromIntegral b / 255
		cMax = maximum [r', g', b']
		cMin = minimum [r', g', b']
		delta = cMax - cMin

		h'
			| cMax == cMin = 0
			| cMax == r' = ((g' - b') / delta) `rMod` 6
			| cMax == g' = (b' - r') / delta + 2
			| cMax == b' = (r' - g') / delta + 4
		h = round h' * 60 `mod` 360
		-- the use of `round` here forces our `a` to be an Integral
		-- is this acceptable?
		s
			| l > 0 = delta / (1 - abs (2 * l - 1))
			| otherwise = 0
		l = (cMax + cMin) / 2

-- a `mod` function for RealFrac types
rMod :: (RealFrac a) => a -> a -> a
rMod a b = a - fromIntegral (truncate $ a / b)

{----------------------------------------------------------------------------------------------------{
                                                                      | Color adjustments
}----------------------------------------------------------------------------------------------------}

adjustColor :: (Integral a, RealFrac b) => (a, b, b) -> (a -> a) -> (b -> b) -> (b -> b) -> (a, b, b)
adjustColor (h, s, l) adjustH adjustS adjustL =
	(adjustH h `mod` 360, clamp $ adjustS s, clamp $ adjustL l)
	where
		clamp = min 100 . max 0

--------------------------------------------------------------------- | Hue

adjustHue :: (Integral a, RealFrac b) => (a, b, b) -> a -> (a, b, b)
adjustHue c x = adjustColor c (+ x) id id

-- Sass doesn't have specific functions that adjust the hue in one direction or the other

--------------------------------------------------------------------- | Saturation

adjustSaturation :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
adjustSaturation c x = adjustColor c id (+ x) id

-- convenient shortcuts
saturate :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
saturate = adjustSaturation

desaturate :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
desaturate c x = adjustSaturation c (-x)

--------------------------------------------------------------------- | Lightness

adjustLightness :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
adjustLightness c x = adjustColor c id id (+ x)

-- convenient shortcuts
lighten :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
lighten = adjustLightness

darken :: (Integral a, RealFrac b) => (a, b, b) -> b -> (a, b, b)
darken c x = adjustLightness c (-x)

{----------------------------------------------------------------------------------------------------{
                                                                      | Color names
}----------------------------------------------------------------------------------------------------}

keywords :: [(String, Color)]
keywords =
	("transparent", Color 0 0 0 0) :
	map (second rgbToColor) keywordRGB

keywordRGB :: [(String, (Channel, Channel, Channel))]
keywordRGB =
	-- standard keywords
	[ ("aliceblue", (240, 248, 255))
	, ("antiquewhite", (250, 235, 215))
	, ("aquamarine", (127, 255, 212))
	, ("azure", (240, 255, 255))
	, ("beige", (245, 245, 220))
	, ("bisque", (255, 228, 196))
	, ("black", (0, 0, 0))
	, ("blanchedalmond", (255, 235, 205))
	, ("blue", (0, 0, 255))
	, ("blueviolet", (138, 43, 226))
	, ("brown", (165, 42, 42))
	, ("burlywood", (222, 184, 135))
	, ("cadetblue", (95, 158, 160))
	, ("chartreuse", (127, 255, 0))
	, ("chocolate", (210, 105, 30))
	, ("coral", (255, 127, 80))
	, ("cornflowerblue", (100, 149, 237))
	, ("cornsilk", (255, 248, 220))
	, ("crimson", (220, 20, 60))
	, ("cyan", (0, 255, 255))
	, ("darkblue", (0, 0, 139))
	, ("darkcyan", (0, 139, 139))
	, ("darkgoldenrod", (184, 134, 11))
	, ("darkgray", (169, 169, 169))
	, ("darkgreen", (0, 100, 0))
	, ("darkkhaki", (189, 183, 107))
	, ("darkmagenta", (139, 0, 139))
	, ("darkolivegreen", (85, 107, 47))
	, ("darkorange", (255, 140, 0))
	, ("darkorchid", (153, 50, 204))
	, ("darkred", (139, 0, 0))
	, ("darksalmon", (233, 150, 122))
	, ("darkseagreen", (143, 188, 143))
	, ("darkslateblue", (72, 61, 139))
	, ("darkslategray", (47, 79, 79))
	, ("darkturquoise", (0, 206, 209))
	, ("darkviolet", (148, 0, 211))
	, ("deeppink", (255, 20, 147))
	, ("deepskyblue", (0, 191, 255))
	, ("dimgray", (105, 105, 105))
	, ("dodgerblue", (30, 144, 255))
	, ("firebrick", (178, 34, 34))
	, ("floralwhite", (255, 250, 240))
	, ("forestgreen", (34, 139, 34))
	, ("gainsboro", (220, 220, 220))
	, ("ghostwhite", (248, 248, 255))
	, ("gold", (255, 215, 0))
	, ("goldenrod", (218, 165, 32))
	, ("gray", (128, 128, 128))
	, ("green", (0, 128, 0))
	, ("greenyellow", (173, 255, 47))
	, ("honeydew", (240, 255, 240))
	, ("hotpink", (255, 105, 180))
	, ("indianred", (205, 92, 92))
	, ("indigo", (75, 0, 130))
	, ("ivory", (255, 255, 240))
	, ("khaki", (240, 230, 140))
	, ("lavender", (230, 230, 250))
	, ("lavenderblush", (255, 240, 245))
	, ("lawngreen", (124, 252, 0))
	, ("lemonchiffon", (255, 250, 205))
	, ("lightblue", (173, 216, 230))
	, ("lightcoral", (240, 128, 128))
	, ("lightcyan", (224, 255, 255))
	, ("lightgoldenrodyellow", (250, 250, 210))
	, ("lightgray", (211, 211, 211))
	, ("lightgreen", (144, 238, 144))
	, ("lightpink", (255, 182, 193))
	, ("lightsalmon", (255, 160, 122))
	, ("lightseagreen", (32, 178, 170))
	, ("lightskyblue", (135, 206, 250))
	, ("lightslategray", (119, 136, 153))
	, ("lightsteelblue", (176, 196, 222))
	, ("lightyellow", (255, 255, 224))
	, ("lime", (0, 255, 0))
	, ("limegreen", (50, 205, 50))
	, ("linen", (250, 240, 230))
	, ("magenta", (255, 0, 255))
	, ("maroon", (128, 0, 0))
	, ("mediumaquamarine", (102, 205, 170))
	, ("mediumblue", (0, 0, 205))
	, ("mediumorchid", (186, 85, 211))
	, ("mediumpurple", (147, 112, 219))
	, ("mediumseagreen", (60, 179, 113))
	, ("mediumslateblue", (123, 104, 238))
	, ("mediumspringgreen", (0, 250, 154))
	, ("mediumturquoise", (72, 209, 204))
	, ("mediumvioletred", (199, 21, 133))
	, ("midnightblue", (25, 25, 112))
	, ("mintcream", (245, 255, 250))
	, ("mistyrose", (255, 228, 225))
	, ("moccasin", (255, 228, 181))
	, ("navajowhite", (255, 222, 173))
	, ("navy", (0, 0, 128))
	, ("oldlace", (253, 245, 230))
	, ("olive", (128, 128, 0))
	, ("olivedrab", (107, 142, 35))
	, ("orange", (255, 165, 0))
	, ("orangered", (255, 69, 0))
	, ("orchid", (218, 112, 214))
	, ("palegoldenrod", (238, 232, 170))
	, ("palegreen", (152, 251, 152))
	, ("paleturquoise", (175, 238, 238))
	, ("palevioletred", (219, 112, 147))
	, ("papayawhip", (255, 239, 213))
	, ("peachpuff", (255, 218, 185))
	, ("peru", (205, 133, 63))
	, ("pink", (255, 192, 203))
	, ("plum", (221, 160, 221))
	, ("powderblue", (176, 224, 230))
	, ("purple", (128, 0, 128))
	, ("red", (255, 0, 0))
	, ("rosybrown", (188, 143, 143))
	, ("royalblue", (65, 105, 225))
	, ("saddlebrown", (139, 69, 19))
	, ("salmon", (250, 128, 114))
	, ("sandybrown", (244, 164, 96))
	, ("seagreen", (46, 139, 87))
	, ("seashell", (255, 245, 238))
	, ("sienna", (160, 82, 45))
	, ("silver", (192, 192, 192))
	, ("skyblue", (135, 206, 235))
	, ("slateblue", (106, 90, 205))
	, ("slategray", (112, 128, 144))
	, ("snow", (255, 250, 250))
	, ("springgreen", (0, 255, 127))
	, ("steelblue", (70, 130, 180))
	, ("tan", (210, 180, 140))
	, ("teal", (0, 128, 128))
	, ("thistle", (216, 191, 216))
	, ("tomato", (255, 99, 71))
	, ("turquoise", (64, 224, 208))
	, ("violet", (238, 130, 238))
	, ("wheat", (245, 222, 179))
	, ("white", (255, 255, 255))
	, ("whitesmoke", (245, 245, 245))
	, ("yellow", (255, 255, 0))
	, ("yellowgreen", (154, 205, 50))

	-- alternate keywords
	, ("aqua", (0, 255, 255))
	, ("darkgrey", (169, 169, 169))
	, ("darkslategrey", (47, 79, 79))
	, ("dimgrey", (105, 105, 105))
	, ("fuchsia", (255, 0, 255))
	, ("grey", (128, 128, 128))
	, ("lightgrey", (211, 211, 211))
	, ("lightslategrey", (119, 136, 153))
	, ("slategrey", (112, 128, 144))

	-- late addition to the spec
	, ("rebeccapurple", (102, 51, 153))
	]
