module Luhny where

import Data.Char (isDigit, digitToInt)
import Data.Foldable (foldl, foldr, toList)
import Data.Functor (fmap)
import Data.Sequence (Seq, (<|), (|>), empty, null, index, takeWhileL, dropWhileL)

import Prelude hiding (minimum, maximum, foldr, foldl, null)

minimum = 14
maximum = 16
masking = 'X'

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '-' = True
isSpace _ = False

type Total = (Int, Int, Seq Int)

data Item
	= Space Char
	| Digit Char Total
	| Other Char
	deriving (Read, Show)

type Stream = Seq Item


double :: Int -> Int
double x = uncurry (+) $ flip divMod 10 $ 2 * x

add :: Bool -> Int -> Int -> Int
add twice x y = (y + z) `mod` 10
	where z = if twice then double x else x


new :: Total
new = (0, 0, empty)

updateTotal :: Int -> Total -> Total
updateTotal number (total, count, zeros) = (total', count', zeros')
	where
		total' = add (odd count) number total
		count' = count + 1
		zeros' = if total' == 0 then count' <| zeros else zeros

maskCount :: Total -> Int
maskCount (_, _, zeros) = if null zeros' then 0 else index zeros' 0
	where zeros' = takeWhileL (>=minimum) $ dropWhileL (>maximum) zeros

maskChar :: Char -> Int -> (Char, Int)
maskChar char count = if count == 0
	then (char, 0)
	else (masking, count - 1)


updateItem :: Int -> (Stream, Bool) -> Item -> (Stream, Bool)
updateItem number (stream, False) item = (stream |> item, False)
updateItem number (stream, True) item = case item of
	Space _ -> (stream |> item, True)
	Other _ -> (stream |> item, False)
	Digit char total -> (stream |> item', True)
		where item' = Digit char $ updateTotal number total


mask :: Item -> (Stream, Int) -> (Stream, Int)
mask item (stream, count) = case item of
	Space _ -> (item <| stream, count)
	Other _ -> (item <| stream, 0)
	Digit char total -> ((Other char') <| stream, count')
		where (char', count') = maskChar char $ max count $ maskCount total


maskStream :: Stream -> Stream
maskStream = fst . foldr mask (empty, 0)

update :: Int -> Stream -> Stream
update number = fst . foldl (updateItem number) (empty, True)

insert :: Char -> Stream -> Stream
insert char stream
	| isDigit char = update (digitToInt char) $ (Digit char new) <| stream
	| isSpace char = (Space char) <| stream
	| otherwise = (Other char) <| stream


fromString :: String -> Stream
fromString = foldr insert empty

toString :: Stream -> String
toString = toList . fmap toChar
	where
		toChar (Space char) = char
		toChar (Other char) = char
		toChar (Digit char _) = char


luhn :: String -> String
luhn = toString . maskStream . fromString


main :: IO ()
main = catch loop done
	where
		done = const $ return ()
		loop = getLine >>= (putStrLn . luhn) >> loop
