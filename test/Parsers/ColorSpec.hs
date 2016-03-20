module Parsers.ColorSpec where

import Test.Hspec

import Text.Parsec (parse)
import Types.Color
import Parsers.Color

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Color Parsers" $ do
		it "Hexadecimal (3 digit)" $
			parse hexColor "" "#333 ignore" `shouldBe` Right (Color 51 51 51 1)
		it "Hexadecimal (6 digit)" $
			parse hexColor "" "#333333 ignore" `shouldBe` Right (Color 51 51 51 1)

		it "RGB" $
			parse rgbColor "" "rgb(51, 51, 51) ignore" `shouldBe` Right (Color 51 51 51 1)
		it "RGBA" $
			parse rgbaColor "" "rgba(51, 51, 51, 0.5) ignore" `shouldBe` Right (Color 51 51 51 0.5)

		it "HSL" $
			parse hslColor "" "hsl(0, 0%, 20%) ignore" `shouldBe` Right (Color 51 51 51 1)
		it "HSLA" $
			parse hslaColor "" "hsla(0, 0%, 20%, 0.5) ignore" `shouldBe` Right (Color 51 51 51 0.5)

		it "Keyword" $
			parse keyword "" "red ignore" `shouldBe` Right (Color 255 0 0 1)

		it "Color (Hexadecimal)" $
			parse color "" "#333 ignore" `shouldBe` Right (Color 51 51 51 1)
		it "Color (RGB)" $
			parse color "" "rgb(51, 51, 51) ignore" `shouldBe` Right (Color 51 51 51 1)
