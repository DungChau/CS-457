-- Dung Chau
-- CS 457 Winter 2012
-- HW 3


--Question 1.A
--Write a function padTo :: Int -> String -> String such that
--padTo n s is a string of length n, padded if necessary with extra
--spaces on the left.

-- This is an additional auxiliary function that append whitespace to the string
addSpace n xs = if n <= 0 || null xs
	then xs
	else addSpace (n - 1) (' ':xs)

-- this is the padTo function which adds spaces to a string then reverse it
--  after that it will truncate n characters and return a right string.
padTo :: Int -> String -> String
padTo n xs = --reverse . take . reverse . addSpace n xs
	reverse str1
		where str1 = take n str2
			where str2 = reverse str3
				where str3 = addSpace n xs

-- Question 1.B 
-- The purpose of this question is to construct a function, multable,
-- that can output (square) multiplication tables of any given size

-- additional auxiliary function that will calculate the space need for display
numbDigit :: [[Integer]] -> Int
numbDigit xss = 
	((length . show) n) + 2
		where n = (xss !! strlen) !! strlen
			where strlen = (length xss) - 1

--format each number into column with the space calculated above 
formatCol :: Int -> [[Integer]] -> [[String]]
formatCol n xss = 
	let 
		yss = map (map show) xss	
	in map (map (padTo n)) yss

--join numbers of each line together 
--an example: map (foldl (++) []) [["1","2"],["3","4"]]
joinLn :: [[String]] -> [String]
joinLn xss = map (foldl (++) []) xss 

-- Ex : (putStr .reverse .('\n':) . reverse) "dsa"
-- use unlines to join all strings together and put '\n' endline
-- character to the end of each new line.
formatEndl :: [String] -> String
formatEndl xss = unlines xss
			
multable  = putStr . showTable . makeTable

makeTable  :: Integer -> [[Integer]]
makeTable n = [ [x * y | x <- [1..n] ] | y <- [1..n]]

showTable  :: [[Integer]] -> String
showTable xss = 
	(formatEndl . joinLn) yss
			where yss = formatCol (numbDigit xss) xss
			
{-
One result of this function:
*Main> multable 7
   1   2   3   4   5   6   7
   2   4   6   8  10  12  14
   3   6   9  12  15  18  21
   4   8  12  16  20  24  28
   5  10  15  20  25  30  35
   6  12  18  24  30  36  42
   7  14  21  28  35  42  49	
-}					

{-
Question 2:
-----------
a) There are many different ways to construct a non-empty list using
only non-empty list enumerations and the ++ operator.  For example,
the list [1,2,3] can be constructed by writing [1,2,3], ([1]++[2,3]),
or (([1]++[2])++[3]) (and these are not the only options).

define a function

  allWays   :: [Integer] -> [String]
  allWays xs = ...

that will produce a list of strings that show all of the possible
ways to build the given list of integers in this way, provided that
the input list is not empty
-}
splits :: [a] -> [([a], [a])]
splits ts = zip (inits ts) (tails ts)

inits :: [a] -> [[a]]
inits [x] = []
inits (x:xs) = map (x:) ([]:inits xs)

tails :: [a] -> [[a]]
tails [x]    = []
tails (x:xs) = xs : tails xs

appString    :: String -> String -> String
appString l r = "(" ++ l ++ "++" ++ r ++ ")"	

layout :: [String] -> IO ()
layout  = putStr
        . unlines
        . zipWith (\n l -> show n ++ ") " ++ l) [1..]

allWays   :: [Integer] -> [String]
allWays [x] = [ "[" ++ (show x) ++ "]" ]
-- I have to concat a [show xs] list to the list in order to generate the original list
allWays xs = [appString l r | (ls,rs) <- splits xs, l <- allWays ls , r <- allWays rs] ++ [show xs]

{-
This is the result of running this function:
*Main> (layout . allWays  ) [1,2,3]
1) ([1]++([2]++[3]))
2) ([1]++[2,3])
3) (([1]++[2])++[3])
4) ([1,2]++[3])
5) [1,2,3]
-}

{- Question 2 B
Write a new function:

  noParens   :: [Integer] -> [String]
  noParens xs = ...

that generates a list of strings showing all of the possible
ways to construct the given input list using only ++ and list
enumeration, without any repetition.

-}

appString' 	:: String -> String -> String
appString'	l r = l ++ "++" ++ r

-- create a list of possible trees by part a code but without parrnthese 
noParens'   :: [Integer] -> [String]
noParens'	[x] = ["[" ++ (show x) ++ "]"]
noParens'	xs = [appString' l r | (ls,rs) <- splits xs, l <- noParens' ls, r <- noParens' rs] ++ [show xs]
-- remove duplicate
remmoveDup :: [String] -> [String]
remmoveDup [] = []
remmoveDup (x:xs) = x : remmoveDup (filter (\y -> not(x == y)) xs)
--main function of this question by calling noParens' and rmDup to remove all repetitive strings 
noParens   :: [Integer] -> [String]
noParens 	xs = let ys = noParens' xs
					in remmoveDup ys






