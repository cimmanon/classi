module Types.Color
{-	( Color(..)
	, hex2Int
	, hslrgb
	, rgbhsl
	, lighten
	, keywords
	)-} where

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
	, green :: Channel
	, blue :: Channel
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

-- TODO: add the rest of the color keywords
keywords :: [(String, Color)]
keywords =
	[ ("white", Color 255 255 255 0)
	, ("black", Color 0 0 0 0)
	]

{-
-- TODO: add the rest of the color keywords
keywords' :: [(String, String)]
keywords' =
	[ ("aliceblue", "#f0f8ff")
	, ("antiquewhite", "#faebd7")
	, ("aquamarine", "#7fffd4")
	, ("azure", "#f0ffff")
	, ("beige", "#f5f5dc")
	, ("bisque", "#ffe4c4")
	, ("black", "#000000")
	, ("blanchedalmond", "#ffebcd")
	, ("blue", "#0000ff")
	, ("blueviolet", "#8a2be2")
	, ("brown", "#a52a2a")
	, ("burlywood", "#deb887")
	, ("cadetblue", "#5f9ea0")
	, ("chartreuse", "#7fff00")
	, ("chocolate", "#d2691e")
	, ("coral", "#ff7f50")
	, ("cornflowerblue", "#6495ed")
	, ("cornsilk", "#fff8dc")
	, ("crimson", "#dc143c")
	, ("cyan", "#00ffff")
	, ("darkblue", "#00008b")
	, ("darkcyan", "#008b8b")
	, ("darkgoldenrod", "#b8860b")
	, ("darkgray", "#a9a9a9")
	, ("darkgreen", "#006400")
	, ("darkkhaki", "#bdb76b")
	, ("darkmagenta", "#8b008b")
	, ("darkolivegreen", "#556b2f")
	, ("darkorange", "#ff8c00")
	, ("darkorchid", "#9932cc")
	, ("darkred", "#8b0000")
	, ("darksalmon", "#e9967a")
	, ("darkseagreen", "#8fbc8f")
	, ("darkslateblue", "#483d8b")
	, ("darkslategray", "#2f4f4f")
	, ("darkturquoise", "#00ced1")
	, ("darkviolet", "#9400d3")
	, ("deeppink", "#ff1493")
	, ("deepskyblue", "#00bfff")
	, ("dimgray", "#696969")
	, ("dodgerblue", "#1e90ff")
	, ("firebrick", "#b22222")
	, ("floralwhite", "#fffaf0")
	, ("forestgreen", "#228b22")
	, ("fuchsia", "#ff00ff")
	, ("gainsboro", "#dcdcdc")
	, ("ghostwhite", "#f8f8ff")
	, ("gold", "#ffd700")
	, ("goldenrod", "#daa520")
	, ("gray", "#808080")
	, ("green", "#008000")
	, ("greenyellow", "#adff2f")
	, ("honeydew", "#f0fff0")
	, ("hotpink", "#ff69b4")
	, ("indianred", "#cd5c5c")
	, ("indigo", "#4b0082")
	, ("ivory", "#fffff0")
	, ("khaki", "#f0e68c")
	, ("lavender", "#e6e6fa")
	, ("lavenderblush", "#fff0f5")
	, ("lawngreen", "#7cfc00")
	, ("lemonchiffon", "#fffacd")
	, ("lightblue", "#add8e6")
	, ("lightcoral", "#f08080")
	, ("lightcyan", "#e0ffff")
	, ("lightgoldenrodyellow", "#fafad2")
	, ("lightgreen", "#90ee90")
	, ("lightgrey", "#d3d3d3")
	, ("lightpink", "#ffb6c1")
	, ("lightsalmon", "#ffa07a")
	, ("lightseagreen", "#20b2aa")
	, ("lightskyblue", "#87cefa")
	, ("lightslategray", "#778899")
	, ("lightsteelblue", "#b0c4de")
	, ("lightyellow", "#ffffe0")
	, ("lime", "#00ff00")
	, ("limegreen", "#32cd32")
	, ("linen", "#faf0e6")
	, ("magenta", "#ff00ff")
	, ("maroon", "#800000")
	, ("mediumaquamarine", "#66cdaa")
	, ("mediumblue", "#0000cd")
	, ("mediumorchid", "#ba55d3")
	, ("mediumpurple", "#9370db")
	, ("mediumseagreen", "#3cb371")
	, ("mediumslateblue", "#7b68ee")
	, ("mediumspringgreen", "#00fa9a")
	, ("mediumturquoise", "#48d1cc")
	, ("mediumvioletred", "#c71585")
	, ("midnightblue", "#191970")
	, ("mintcream", "#f5fffa")
	, ("mistyrose", "#ffe4e1")
	, ("moccasin", "#ffe4b5")
	, ("navajowhite", "#ffdead")
	, ("navy", "#000080")
	, ("oldlace", "#fdf5e6")
	, ("olive", "#808000")
	, ("olivedrab", "#6b8e23")
	, ("orange", "#ffa500")
	, ("orangered", "#ff4500")
	, ("orchid", "#da70d6")
	, ("palegoldenrod", "#eee8aa")
	, ("palegreen", "#98fb98")
	, ("paleturquoise", "#afeeee")
	, ("palevioletred", "#db7093")
	, ("papayawhip", "#ffefd5")
	, ("peachpuff", "#ffdab9")
	, ("peru", "#cd853f")
	, ("pink", "#ffc0cb")
	, ("plum", "#dda0dd")
	, ("powderblue", "#b0e0e6")
	, ("purple", "#800080")
	, ("red", "#ff0000")
	, ("rebeccapurple", "#663399")
	, ("rosybrown", "#bc8f8f")
	, ("royalblue", "#4169e1")
	, ("saddlebrown", "#8b4513")
	, ("salmon", "#fa8072")
	, ("sandybrown", "#f4a460")
	, ("seagreen", "#2e8b57")
	, ("seashell", "#fff5ee")
	, ("sienna", "#a0522d")
	, ("silver", "#c0c0c0")
	, ("skyblue", "#87ceeb")
	, ("slateblue", "#6a5acd")
	, ("slategray", "#708090")
	, ("snow", "#fffafa")
	, ("springgreen", "#00ff7f")
	, ("steelblue", "#4682b4")
	, ("tan", "#d2b48c")
	, ("teal", "#008080")
	, ("thistle", "#d8bfd8")
	, ("tomato", "#ff6347")
	, ("turquoise", "#40e0d0")
	, ("violet", "#ee82ee")
	, ("wheat", "#f5deb3")
	, ("white", "#ffffff")
	, ("whitesmoke", "#f5f5f5")
	, ("yellow", "#ffff00")
	, ("yellowgreen", "#9acd32")
	]

alternateKeywords :: [(String, String)]
alternateKeywords =
	[ ("aqua", "#00ffff")
	, ("darkgrey", "#a9a9a9")
	, ("darkslategrey", "#2f4f4f")
	, ("dimgrey", "#696969")
	, ("fuchsia", "#ff00ff")
	, ("grey", "#808080")
	, ("lightgrey", "#d3d3d3")
	, ("lightslategrey", "#778899")
	, ("slategrey", "#708090")
	]
-}
