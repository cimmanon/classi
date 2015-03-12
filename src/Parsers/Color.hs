module Parsers.Color where

color :: Parser Color
color =
	hexColor <|> rgbColor
	where

hexColor :: Parser Color
hexColor = do
	satisfy (== '#')
	x <- many1 hexDigit
	case x of
		r1:r2:g1:g2:b1:b2:[] -> return $ Color (hex2Int [r1, r2]) (hex2Int [g1, g2]) (hex2Int [b1, b2]) 0
		r:g:b:[] -> return $ Color (hex2Int [r, r]) (hex2Int [g, g]) (hex2Int [b, b]) 0
		_ -> unexpected "Invalid format for hexadecimal color (eg. #333 or #333333)"

rgbColor :: Parser Color
rgbColor = do
	Text.Parsec.string "rgb("
	spaces
	colors <- csv digit
	spaces
	satisfy (== ')')
	case colors of
		r:g:b:[] -> return $ Color (read r) (read g) (read b) 0
		_ -> unexpected $ "Expected 3 colors values for rgb format, received " <> (show $ length colors)
{-
rgbaColor :: Parser Color
rgbaColor = do
	string "rgba(" <* spaces
	colors <- csv digit
	satisfy (== ')')
	case colors of
		r:g:b:a:[] -> return $ Color (read r) (read g) (read b) 0
		_ -> unexpected "Invalid format for rgba color (eg. rgb(255, 255, 255, 0.5))"
-}

hslColor :: Parser [Int]
hslColor = do
	Text.Parsec.string "hsl("
	spaces
	h <- many1 digit
	s <- value <$> do commaDelimiter *> percentage
	l <- value <$> do commaDelimiter *> percentage
	satisfy (== ')')
	return $ hslrgb