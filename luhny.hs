module Luhny where

import Data.Char (isDigit, digitToInt)
import Data.Foldable (foldl, foldr)
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

data Thing = Thing {
	total :: Int,
	count :: Int,
	zeros :: Seq Int
} deriving (Read, Show)

data Item
	= Char Char
	| Item {
		char :: Char,
		thing :: Thing
	} deriving (Read, Show)

type Stream = Seq Item


double_plus :: Bool -> Int -> Int -> Int
double_plus double x y = let 
	(d, m) = (2 * x) `divMod` 10
	z = if double then d + m else x
	in (y + z) `mod` 10


new :: Thing
new = Thing {
	total = 0,
	count = 0,
	zeros = Seq.empty
}

add :: Int -> Thing -> Thing
add number thing@(Thing { total = total, count = count, zeros = zeros }) = let
	total' = double_plus (odd count) number total
	count' = count + 1
	zeros' = if total' == 0 then count' <| zeros else zeros
	in Thing { total = total', count = count', zeros = zeros' }

foo :: Thing -> Int
foo Thing { zeros = zeros } = let
	zeros' = Seq.takeWhileL (>=minimum) $ Seq.dropWhileL (>maximum) zeros
	in if Seq.null zeros' then 0 else Seq.index zeros' 0


item :: Char -> Item
item char = Item { char = char, thing = new }

incr :: Int -> Item -> Item
incr num item = case item of
	Char _ -> item
	otherwise -> item { thing = add num $ thing item }

eval :: Item -> Int
eval item = case item of
	Char _ -> 0
	otherwise -> foo $ thing item

snarf :: Item -> (Stream, Int) -> (Stream, Int)
snarf item (stream, count) = case item of
	Char _ -> (item <| stream, count)
	otherwise -> let
		count' = max count $ eval item
		in if count' == 0
			then ((Char $ char item) <| stream, 0)
			else ((Char masking) <| stream, count' - 1)

convert :: Stream -> Stream
convert = fst . foldr snarf (Seq.empty, 0)


insert :: Int -> Stream -> Stream
insert num = fmap $ incr num

insert_stream :: Char -> Stream -> Stream
insert_stream char stream
	| isDigit char = insert (digitToInt char) $ (item char) <| stream
	| isSpace char = (Char char) <| stream
	| otherwise = (Char char) <| stream

inject :: String -> Stream
inject = foldr insert_stream Seq.empty


insert_string :: Item -> String -> String
insert_string (Char char) = (char:)
insert_string (Item { char = char }) = (char:)

extract :: Stream -> String
extract = foldr insert_string ""


luhn :: String -> String
luhn = extract . convert . inject


main :: IO ()
main = catch loop done
	where
		done = const $ return ()
		loop = getLine >>= (putStrLn . luhn) >> loop
