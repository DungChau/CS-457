
powerOfTwo n = product (take n (repeat 2))


logTwo v = head ([ n | n <- [1 .. v], v <= powerOfTwo n])

copy :: Int -> a -> [a]
copy n x = take n (repeat x)

multiApply :: (a -> a) -> Int -> a -> a
multiApply f n x = last ((take (n + 1) (iterate f x)))


myZipWith f xs ys = [f x y | (x,y) <- zip xs ys]

adjacents xs = zip xs (tail xs)

nextRow xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]

comb n r = product [1..n] / (product [1 .. (n-r)] * product [1..r])

comb' n r =  last ((take (n + 1) (iterate nextRow [1]))) !! r

cons xs ys = map (xs==) ys



