{-# LANGUAGE OverloadedStrings, GADTs #-}

module Types
	( module Types.Color
--	, ListDelimiter(..)
	, module Types.Dimension
	) where

import Data.Text (Text)

import Types.Color
import Types.Dimension

data ListDelimiter = Comma | Space

data Expression = Expression [Text]

data CssType a where
	CssString :: Text -> CssType Text
	CssColor :: Color -> CssType Color
	CssDimension :: Dimension -> CssType Dimension
	CssCalc :: Expression -> CssType Expression
	-- ^ TODO: figure out how to actually handle calc() expressions
	-- CssCalc is going to need an instance of Num and Fractal
	-- https://www.w3.org/TR/css3-values/#calc-notation
	CssList :: ([CssType a], ListDelimiter) -> CssType ([CssType a], ListDelimiter)
	CssMapping :: [(Text, CssType a)] -> CssType [(Text, CssType a)]
	-- ^ the "keys" for mappings need to be restricted to Text to avoid confusion when using Color names (eg. red)

-- we have to make our own instances for GADTs, we can't just derive it like normal
-- TODO: make nicer show instances
instance (Show a) => Show (CssType a) where
	show (CssString s) = show s
	show (CssColor c) = show c
	show (CssDimension l) = show l
	show (CssCalc e) = show e
	show (CssList xs) = show xs
	show (CssMapping xs) = show xs

{-
lighten :: CssType Color -> CssType Color
lighten = id

lighten' :: CssType Color -> Color
lighten' (CssColor c) = c
-}

{----------------------------------------------------------------------------------------------------{
                                                                      | Color
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
                                                                      | Dimension
}----------------------------------------------------------------------------------------------------}

