{-# LANGUAGE OverloadedStrings, GADTs #-}

module Types
	( module Types.Color
--	, ListDelimiter(..)
	, module Types.Length
	) where

import Types.Color
import Types.Length

-- Going to want to have a mapping type here as well, but that can come later
--data Value = Color | Length | Int | CString | CList

data ListDelimiter = Comma | Space

data CssType a where
	CssString :: String -> CssType String
	CssColor :: Color -> CssType Color
	CssLength :: Length -> CssType Length
	CssList :: ([CssType a], ListDelimiter) -> CssType ([CssType a], ListDelimiter)
	CssMapping :: [(String, CssType a)] -> CssType [(String, CssType a)]
	-- ^ the "keys" for mappings need to be restricted to String to avoid confusion when using Color names (eg. red)

-- we have to make our own instances for GADTs, we can't just derive it like normal
-- TODO: make nicer show instances
instance (Show a) => Show (CssType a) where
	show (CssString s) = show s
	show (CssColor c) = show c
	show (CssLength l) = show l
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
                                                                      | Length
}----------------------------------------------------------------------------------------------------}

