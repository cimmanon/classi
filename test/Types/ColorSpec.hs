module Types.ColorSpec where

import Test.Hspec

import Types.Color

main :: IO ()
main = hspec spec

-- for testing HSL -> RGB conversion
rgbhslrgb :: Color -> Color
rgbhslrgb c =
	let
		(h, s, l) = rgbhsl (red c) (green c) (blue c)
	in
		hslrgb h s l

spec :: Spec
spec = do
	let
		-- neutrals
		white = Color 255 255 255 0
		black = Color 0 0 0 0
		silver = Color 192 192 192 0
		gray = Color 128 128 128 0

		-- colors
		red = Color 255 0 0 0
		lime = Color 0 255 0 0
		blue = Color 0 0 255 0
		yellow = Color 255 255 0 0
		cyan = Color 0 255 255 0
		magenta = Color 255 0 255 0
		maroon = Color 128 0 0 0
		olive = Color 128 128 0 0
		green = Color 0 128 0 0
		purple = Color 128 0 128 0
		teal = Color 0 128 128 0
		navy = Color 0 0 128 0

	describe "HSL to RGB" $ do
		it "Red" $
			hslrgb 0 100 50 `shouldBe` red
		it "Lime" $
			hslrgb 120 100 50 `shouldBe` lime
		it "Blue" $
			hslrgb 240 100 50 `shouldBe` blue
		it "Yellow" $
			hslrgb 60 100 50 `shouldBe` yellow
		it "Cyan" $
			hslrgb 180 100 50 `shouldBe` cyan
		it "Magenta" $
			hslrgb 300 100 50 `shouldBe` magenta
		it "Maroon" $
			hslrgb 0 100 25 `shouldBe` maroon
		it "Olive" $
			hslrgb 60 100 25 `shouldBe` olive
		it "Green" $
			hslrgb 120 100 25 `shouldBe` green
		it "Purple" $
			hslrgb 300 100 25 `shouldBe` purple
		it "Teal" $
			hslrgb 180 100 25 `shouldBe` teal
		it "Navy" $
			hslrgb 240 100 25 `shouldBe` navy

	describe "RGB to HSL" $ do
		it "Red" $
			rgbhslrgb red `shouldBe` red
		it "Lime" $
			rgbhslrgb lime `shouldBe` lime
		it "Blue" $
			rgbhslrgb blue `shouldBe` blue
		it "Yellow" $
			rgbhslrgb yellow `shouldBe` yellow
		it "Cyan" $
			rgbhslrgb cyan `shouldBe` cyan
		it "Magenta" $
			rgbhslrgb magenta `shouldBe` magenta
		it "Maroon" $
			rgbhslrgb maroon `shouldBe` maroon
		it "Olive" $
			rgbhslrgb olive `shouldBe` olive
		it "Green" $
			rgbhslrgb green `shouldBe` green
		it "Purple" $
			rgbhslrgb purple `shouldBe` purple
		it "Teal" $
			rgbhslrgb teal `shouldBe` teal
		it "Navy" $
			rgbhslrgb navy `shouldBe` navy

	describe "Adjust Hue" $ do
		it "Increase hue of Red by 10deg" $
			adjustHue red 10 `shouldBe` Color 255 43 0 0
		it "Increase hue of Red by 20deg" $
			adjustHue red 20 `shouldBe` Color 255 85 0 0
		it "Increase hue of Red by 30deg" $
			adjustHue red 30 `shouldBe` Color 255 128 0 0
		it "Increase hue of Red by 40deg" $
			adjustHue red 40 `shouldBe` Color 255 170 0 0
		it "Increase hue of Red by 50deg" $
			adjustHue red 50 `shouldBe` Color 255 213 0 0
		it "Increase hue of Red by 60deg" $
			adjustHue red 60 `shouldBe` Color 255 255 0 0
		it "Increase hue of Red by 80deg" $
			adjustHue red 80 `shouldBe` Color 170 255 0 0
		it "Increase hue of Red by 100deg" $
			adjustHue red 100 `shouldBe` Color 85 255 0 0

	describe "Adjust Saturation" $ do
		it "Decrease saturation of Red by 10%" $
			desaturate red 10 `shouldBe` Color 242 13 13 0
		it "Decrease saturation of Red by 20%" $
			desaturate red 20 `shouldBe` Color 230 26 26 0
		it "Decrease saturation of Red by 30%" $
			desaturate red 30 `shouldBe` Color 217 38 38 0
		it "Decrease saturation of Red by 40%" $
			desaturate red 40 `shouldBe` Color 204 51 51 0
		it "Decrease saturation of Red by 50%" $
			desaturate red 50 `shouldBe` Color 191 64 64 0

	describe "Adjust Lightness" $ do
		let
			grey = Color 50 50 50 1

		it "Increase lightness of Grey by 10%" $
			lighten grey 10 `shouldBe` Color 76 76 76 0
		it "Increase lightness of Grey by 20%" $
			lighten grey 20 `shouldBe` Color 101 101 101 0
		it "Increase lightness of Grey by 30%" $
			lighten grey 30 `shouldBe` Color 127 127 127 0
		it "Increase lightness of Grey by 40%" $
			lighten grey 40 `shouldBe` Color 152 152 152 0
		it "Increase lightness of Grey by 50%" $
			lighten grey 50 `shouldBe` Color 178 178 178 0

{-
-- TODO: run this entire list
hsl2rgbTest :: [(Color, Color)]
hsl2rgbTest =
	[ (hslrgb 0 50 50, Color 191 64 64 0)
	, (hslrgb 0 0 50, Color 128 128 128 0)
	, (hslrgb 20 15 30, Color 88 73 65 0)
	, (hslrgb 50 5 50, Color 132 134 121 0)
	, (hslrgb 50 10 50, Color 140 136 115 0)
	, (hslrgb 100 50 50, Color 106 191 64 0)
	, (hslrgb 100 50 75, Color 181 223 159 0)
	, (hslrgb 100 75 75, Color 175 239 143 0)
	]
-}
