> module StringSearch where

> import Data.List hiding (lookup)
> import Data.Map hiding (findIndex)
> import Test.QuickCheck
> import Test.QuickCheck.Gen
> import System.Random	

--------------------------------------------------------------------

bruteforce gets 2 params: p is pattern and another one is text to be searched. 

====================================================================

isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

====================================================================

 instance Arbitrary Char where
   arbitrary = oneof (map return ['a'..'z'])

====================================================================

> searchStr     :: String -> String -> Maybe Int
> searchStr pattern = findIndex (isPrefixOf pattern) . tails

--------------------------------------------------------------------

> bruteforce	:: String -> String -> Maybe Int
> bruteforce ns hs = bruteforce' 0 0 ns where
> 		(patternLen,textLen) = (length ns, length hs)	
>		bruteforce' m  n []  = Just n
>		bruteforce' m  n (p:ps)  | n + patternLen > textLen = Nothing
>		                         | p == hs !! m = bruteforce' (m + 1) n ps
>									                  | otherwise = bruteforce' (n + 1) (n + 1) ns

--------------------------------------------------------------------

shiftTable receives a string `pattern` then return a map of char int  

> shiftTable     :: String -> Map Char Int
> shiftTable pattern = fromList $ zip (init pattern) [patternLen - 1,patternLen - 2..] where
>                          patternLen = length pattern

> horspool  ::  String -> String -> Maybe Int
> horspool ps ts = horspool' (patternLen - 1) (patternLen - 1) pattern where
>		(patternLen,textLen,pattern,table) = (length ps, length ts, reverse ps, shiftTable ps)
>		horspool' l a [] = Just (l + 1)
> 		horspool' l a (p:ps)	| a >= textLen || a < 0 = Nothing
>								| p == ts !! l = horspool' (l - 1) a ps
>								| otherwise = horspool' anchor anchor pattern where
>								anchor = a + getSChar (ts !! a)
>								getSChar c = case Data.Map.lookup c table of
>									Just n -> n
>									Nothing -> patternLen	

> prop_search =
>		(forAll (ranDomLs 2) $ \ps ->
>		forAll (ranDomLs 10000) $ \ts -> 
>		classify (searchStr ps ts == Nothing) "Nothing"
>			(classify (searchStr ps ts /= Nothing) "Found"
>				(searchStr ps ts == bruteforce ps ts)))
 

> ranDomLs :: Int -> Gen [Char]
> ranDomLs size = vectorOf size $ elements ['a'..'z']

> ranDomLs' :: Int -> String ->Gen [Char]
> ranDomLs' size alphabet = vectorOf size $ elements alphabet

> prop_search' = 
>	(forAll (ranDomLs 2) $ \ps ->
>	forAll (ranDomLs 10000) $ \ts ->
>	classify (searchStr ps ts == Nothing) "Nothing"
>		(classify (searchStr ps ts /= Nothing) "Found"
>			(searchStr ps ts == horspool ps ts)))


> prop_bruteforce m as =
>		(forAll (ranDomLs' m as) $ \ps ->
>		forAll (ranDomLs' 10000 as) $ \ts -> 
>		classify (searchStr ps ts == Nothing) "Nothing"
>			(classify (searchStr ps ts /= Nothing) "Found"
>				(searchStr ps ts == bruteforce ps ts)))

> prop_horspool m as = 
>		(forAll (ranDomLs' m as) $ \ps ->
>		forAll (ranDomLs' 10000 as) $ \ts -> 
>		classify (searchStr ps ts == Nothing) "Nothing"
>			(classify (searchStr ps ts /= Nothing) "Found"
>				(searchStr ps ts == horspool ps ts)))	

