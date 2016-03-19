module Parsers.SelectorSpec where

import Test.Hspec

import Text.Parsec (parse)
import Parsers.Selector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Selectors" $ do
		it "Element" $
			parse elementSelector "" "body ignore" `shouldBe` Right "body"

		it "ID" $
			parse idSelector "" "#foo ignore" `shouldBe` Right "#foo"
		it "ID with Unicode" $
			parse idSelector "" "#\\03 3foo ignore" `shouldBe` Right "#\\03 3foo"

		it "Class" $
			parse classSelector "" ".foo ignore" `shouldBe` Right ".foo"
		it "Class with Unicode" $
			parse classSelector "" ".\\03 3foo ignore" `shouldBe` Right ".\\03 3foo"

		it "Pseudo Class (single colon)" $
			parse pseudoSelector "" ":foo ignore" `shouldBe` Right ":foo"
		it "Pseudo Class (single colon, hyphenated)" $
			parse pseudoSelector "" ":last-child ignore" `shouldBe` Right ":last-child"
		it "Pseudo Class (single colon) w/ function" $
			parse pseudoSelector "" ":foo() ignore" `shouldBe` Right ":foo()"
		it "Pseudo Class (single colon) w/ function & args" $
			parse pseudoSelector "" ":foo(2n) ignore" `shouldBe` Right ":foo(2n)"
		it "Pseudo Element (double colon)" $
			parse pseudoSelector "" "::foo ignore" `shouldBe` Right "::foo"
		it "Pseudo Element (double colon) w/ function" $
			parse pseudoSelector "" "::foo() ignore" `shouldBe` Right "::foo()"
		it "Pseudo Element (double colon) w/ function & args" $
			parse pseudoSelector "" "::foo(2n) ignore" `shouldBe` Right "::foo(2n)"

		it "Simple Selectors" $
			parse simpleSelectors "" "body#foo.bar:last-child::after ignore" `shouldBe` Right ["body", "#foo", ".bar", ":last-child", "::after"]
