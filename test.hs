data Bit = O | I    deriving Show

type BinNum = [Bit]

{- Question a
-}
toBinNum   :: Integer -> BinNum
toBinNum	n 	| n==0 = [O]
				| even 	n = [O] ++ toBinNum halfOfN
				| odd 	n = [I] ++ toBinNum halfOfN
				where halfOfN = n `div` 2
				
  	