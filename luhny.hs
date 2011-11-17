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
double_plus double x y = let
	f = if double then twice else id
	z = f x
	in (y + z) `mod` 10


new :: Total
new = (0, 0, Seq.empty)

add :: Int -> Total -> Total
add number (total, count, zeros) = let
	total' = double_plus (odd count) number total
	count' = count + 1
	zeros' = if total' == 0 then count' <| zeros else zeros
	in (total', count', zeros')

foo :: Total -> Int
foo (_, _, zeros) = let
	zeros' = Seq.takeWhileL (>=minimum) $ Seq.dropWhileL (>maximum) zeros
	in if Seq.null zeros' then 0 else Seq.index zeros' 0


item :: Char -> Item
item char = Digit char new

incr :: Int -> Item -> Item
incr num item = case item of
	Digit char total -> Digit char $ add num total
	otherwise -> item

eval :: Item -> Int
eval item = case item of
	Digit _ total -> foo total
	otherwise -> 0

snarf :: Item -> (Stream, Int) -> (Stream, Int)
snarf item (stream, count) = case item of
	Space _ -> (item <| stream, count)
	Other _ -> (item <| stream, count)
	Digit char total -> let
		count' = max count $ eval item
		in if count' == 0
			then ((Other char) <| stream, 0)
			else ((Other masking) <| stream, count' - 1)

convert :: Stream -> Stream
convert = fst . foldr snarf (Seq.empty, 0)


insert :: Int -> Stream -> Stream
insert num = fmap $ incr num

insert_stream :: Char -> Stream -> Stream
insert_stream char stream
	| isDigit char = insert (digitToInt char) $ (item char) <| stream
	| isSpace char = (Space char) <| stream
	| otherwise = (Other char) <| stream

inject :: String -> Stream
inject = foldr insert_stream Seq.empty


insert_string :: Item -> String -> String
insert_string (Space char) = (char:)
insert_string (Other char) = (char:)
insert_string (Digit char _) = (char:)

extract :: Stream -> String
extract = foldr insert_string ""


luhn :: String -> String
luhn = extract . convert . inject


main :: IO ()
main = catch loop done
	where
		done = const $ return ()
		loop = getLine >>= (putStrLn . luhn) >> loop
