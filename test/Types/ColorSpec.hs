module Types.ColorSpec where

import Test.Hspec

import Types.Color

main :: IO ()
main = hspec spec

-- for testing HSL -> RGB conversion
rgbhslrgb :: (Channel, Channel, Channel) -> (Channel, Channel, Channel)
rgbhslrgb = hslrgb . rgbhsl

spec :: Spec
spec = do
	let
		-- neutrals
		white = (255, 255, 255)
		black = (0, 0, 0)
		silver = (192, 192, 192)
		gray = (128, 128, 128)

		-- colors
		red = (255, 0, 0)
		lime = (0, 255, 0)
		blue = (0, 0, 255)

		yellow = (255, 255, 0)
		cyan = (0, 255, 255)
		magenta = (255, 0, 255)

		maroon = (128, 0, 0)
		green = (0, 128, 0)
		navy = (0, 0, 128)

		olive = (128, 128, 0)
		purple = (128, 0, 128)
		teal = (0, 128, 128)

	-- test data pulled from http://www.rapidtables.com/convert/color/hsl-to-rgb.htm
	describe "HSL to RGB" $ do
		it "Red" $
			hslrgb (0, 100, 50) `shouldBe` red
		it "Lime" $
			hslrgb (120, 100, 50) `shouldBe` lime
		it "Blue" $
			hslrgb (240, 100, 50) `shouldBe` blue
		it "Yellow" $
			hslrgb (60, 100, 50) `shouldBe` yellow
		it "Cyan" $
			hslrgb (180, 100, 50) `shouldBe` cyan
		it "Magenta" $
			hslrgb (300, 100, 50) `shouldBe` magenta
		it "Maroon" $
			hslrgb (0, 100, 25) `shouldBe` maroon
		it "Green" $
			hslrgb (120, 100, 25) `shouldBe` green
		it "Navy" $
			hslrgb (240, 100, 25) `shouldBe` navy
		it "Olive" $
			hslrgb (60, 100, 25) `shouldBe` olive
		it "Purple" $
			hslrgb (300, 100, 25) `shouldBe` purple
		it "Teal" $
			hslrgb (180, 100, 25) `shouldBe` teal

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
		it "Green" $
			rgbhslrgb green `shouldBe` green
		it "Navy" $
			rgbhslrgb navy `shouldBe` navy
		it "Olive" $
			rgbhslrgb olive `shouldBe` olive
		it "Purple" $
			rgbhslrgb purple `shouldBe` purple
		it "Teal" $
			rgbhslrgb teal `shouldBe` teal

	describe "Adjust Hue" $ do
		it "Increase hue of Red by 10deg" $
			adjustHue (rgbhsl red) 10 `shouldBe` (10, 100, 50)
		it "Increase hue of Red by 20deg" $
			adjustHue (rgbhsl red) 20 `shouldBe` (20, 100, 50)
		it "Increase hue of Red by 30deg" $
			adjustHue (rgbhsl red) 30 `shouldBe` (30, 100, 50)
		it "Increase hue of Red by 40deg" $
			adjustHue (rgbhsl red) 40 `shouldBe` (40, 100, 50)
		it "Increase hue of Red by 50deg" $
			adjustHue (rgbhsl red) 50 `shouldBe` (50, 100, 50)
		it "Increase hue of Red by 60deg" $
			adjustHue (rgbhsl red) 60 `shouldBe` (60, 100, 50)
		it "Increase hue of Red by 80deg" $
			adjustHue (rgbhsl red) 80 `shouldBe` (80, 100, 50)
		it "Increase hue of Red by 100deg" $
			adjustHue (rgbhsl red) 100 `shouldBe` (100, 100, 50)

	describe "Adjust Saturation" $ do
		it "Decrease saturation of Red by 10%" $
			desaturate (rgbhsl red) 10 `shouldBe` (0, 90, 50)
		it "Decrease saturation of Red by 20%" $
			desaturate (rgbhsl red) 20 `shouldBe` (0, 80, 50)
		it "Decrease saturation of Red by 30%" $
			desaturate (rgbhsl red) 30 `shouldBe` (0, 70, 50)
		it "Decrease saturation of Red by 40%" $
			desaturate (rgbhsl red) 40 `shouldBe` (0, 60, 50)
		it "Decrease saturation of Red by 50%" $
			desaturate (rgbhsl red) 50 `shouldBe` (0, 50, 50)

	describe "Adjust Lightness" $ do
		let
			grey = (50, 50, 50)

		it "Increase lightness of Grey by 10%" $
			lighten grey 10 `shouldBe` (50, 50, 60)
		it "Increase lightness of Grey by 20%" $
			lighten grey 20 `shouldBe` (50, 50, 70)
		it "Increase lightness of Grey by 30%" $
			lighten grey 30 `shouldBe` (50, 50, 80)
		it "Increase lightness of Grey by 40%" $
			lighten grey 40 `shouldBe` (50, 50, 90)
		it "Increase lightness of Grey by 50%" $
			lighten grey 50 `shouldBe` (50, 50, 100)

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
