module Luhny where

import Data.Char (isDigit, digitToInt)
import Data.Foldable (foldl, foldr, toList)
import Data.Functor (fmap)
import Data.Sequence (Seq, (<|), (|>))

import qualified Data.Sequence as Seq

import Prelude hiding (minimum, maximum, foldr, foldl)

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


twice :: Int -> Int
twice n = uncurry (+) $ flip divMod 10 $ 2 * n

double_plus :: Bool -> Int -> Int -> Int
double_plus double x y = (y + z) `mod` 10
	where z = if double then twice x else x


new :: Total
new = (0, 0, Seq.empty)

add :: Int -> Total -> Total
add number (total, count, zeros) = (total', count', zeros')
	where
		total' = double_plus (odd count) number total
		count' = count + 1
		zeros' = if total' == 0 then count' <| zeros else zeros

maskCount :: Total -> Int
maskCount (_, _, zeros) = if Seq.null zeros' then 0 else Seq.index zeros' 0
	where zeros' = Seq.takeWhileL (>=minimum) $ Seq.dropWhileL (>maximum) zeros

maskChar :: Char -> Int -> (Char, Int)
maskChar char num = if num == 0
	then (char, 0)
	else (masking, num - 1)


incr :: Int -> Item -> Item
incr num item = case item of
	Digit char total -> Digit char $ add num total
	otherwise -> item


mask :: Item -> (Stream, Int) -> (Stream, Int)
mask item (stream, count) = case item of
	Space _ -> (item <| stream, count)
	Other _ -> (item <| stream, count)
	Digit char total -> ((Other char') <| stream, count')
		where (char', count') = maskChar char $ max count $ maskCount total


maskStream :: Stream -> Stream
maskStream = fst . foldr mask (Seq.empty, 0)

update :: Int -> Stream -> Stream
update num = fmap $ incr num

insert :: Char -> Stream -> Stream
insert char stream
	| isDigit char = update (digitToInt char) $ (Digit char new) <| stream
	| isSpace char = (Space char) <| stream
	| otherwise = (Other char) <| stream


fromString :: String -> Stream
fromString = foldr insert Seq.empty

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
