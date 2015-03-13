module Types.ColorSpec where

import Test.Hspec

import Types.Color

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Color creation & conversion" $ do
		it "color1" $
			hslrgb 0 50 50 `shouldBe` Color 191 64 64 0

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
