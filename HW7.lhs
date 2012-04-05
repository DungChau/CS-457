********************************************************************************

Dung Chau

CS 457 Winter 2012

Homework 7

********************************************************************************


> module HW7 where

> import IOActions hiding (find)
> import System.IO

--------------------------------------------------------------------------------

QUESTION 1: Use online sources or inspect the files that are already installed 
on your computer to give brief documentation, in your own words, for the find 
function that is defined in Data.List. 

First of all, the definition of find in Data.List:

find            :: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

The find function in Data.List takes a predicate as the first argument and then 
apply it to a list which is the second argument using filter function. The funct
-ion will return the first element in the list that satisfy the predicate or it 
will return nothing if there is no element satisfies that predicate. 
For example:

Prelude Data.List> find (>3) [1..10]
Just 4 

or it returns Nothing if no element found to be matched the predicate:

Prelude Data.List> find (>11) [1..10]
Nothing

--------------------------------------------------------------------------------

QUESTION 2: Explain briefly how the code for find works, and comment on the need 
for the local definition of notDotDot.

> find       :: FilePath -> IO [FilePath] 
> find path  = doesDirectoryExist path >>= \isDir ->
>              case isDir of
>                True   -> getDirectoryContents path
>                          >>= inIO (filter notDotDot)
>                          >>= mapM (find . (path </>))
>                          >>= inIO ((path :) . concat)
>                False  -> return [path]
>   where notDotDot name = name /= "." && name /= ".."
                 
The find function above eccepts a file path as its argument and print out a list 
of all file paths of all files and directories inside of that filepath argument. 
If that filepath does not exist then it returns the filepath argument only.

First of all, it checks if the filepath exists or not which produces True or Fal
-se result by the function doesDirectoryExist. if the filepath does exist, it 
then print out all files in that directory. Also it will recursively process all 
the sub-directories of the directory specified by the filepath and does the same 
thing of print out all files in those sub-directories.  

Before calling recursively find function to find the sub-directories we need to 
filter out "." and ".." cases which are current and parent directory. If we do not 
filter them out, then the call chain will be an infinity loop. 

--------------------------------------------------------------------------------

QUESTION 3: The declaration infixl -| specifies that the -| operator should be 
treated as a function that associates (or groups) to the left. Explain why this 
is necessary here.

> infixl -|

> (-|)      :: IO [FilePath] -> (FilePath -> IO Bool) -> IO [FilePath]
> g -| p    = g >>= filterIO p

> filterIO  :: (a -> IO Bool) -> [a] -> IO [a]
> filterIO p []     = return []
> filterIO p (x:xs) = do b <- p x
>                        if b then filterIO p xs >>= inIO (x:)
>                             else filterIO p xs 

We can examine an expression: find dir -| filt1 -| filt2. That expression starts
at find dir and produces a list of all directories and files in the specified 
directory. Then it feeds the list to filt1. The result again fed to filt2. Suppose
that we group the expression from the right; that is : find dir -| (filt1 -| filt2)
As the result filt1 output fed to filt2 but at this time filt1 has no output yet 
to give to filt2. Thus we get an error.


--------------------------------------------------------------------------------

QUESTION 4: Show how the expression above can be modified to verify that all of 
the FilePaths in the final list do indeed, have an even number of characters in 
their name. 

> name       :: (FilePath -> Bool) -> FilePath -> IO Bool
> name p f   = return (p f)

> haskellFiles = name ("hs" `isSuffixOf`)

The expression is:

find "." -| name (even . length) >>= mapM_ putStrLn

After running this expression we run:

find "." -| name (even . length)

this results a IO [FilePath]

Then we can use the result to feed:

map (even . length)

if the final result is all True then the final list is a list of file paths whose
names are even number of characters. Now we have to glue 2 pieces together by 
using inIO () to turn map (even . length) to IO actions.

The expression becomes:

find "." -| name (even . length) >>= inIO (map ((even . length)))

Test:

*HW7> find "." -| name (even . length)>>= mapM_ putStrLn 
./HW1CS457.txt
./HW2CS457.txt
./HW3.hs
./HW4.hs
*HW7> find "." -| name (even . length) >>= inIO (map ((even . length)))
[True,True,True,True]

--------------------------------------------------------------------------------

The size function can be defined as follows:

> size      :: (Integer -> Bool) -> FilePath -> IO Bool
> size p f  = do b <- doesFileExist f
>                if b then do h <- openFile f ReadMode
>                             l <- hFileSize h
>                             hClose h
>                             return (p l)
>                     else return False

QUESTION 5: Define a function of the following type:

Basically this contents function is similiar to size function above but now we 
use readFile instead to read the content of a text file to varibale c. However,
we need to check if the file does exist or not by using doesFileExist on the
FilePath f. After reading the content to c we apply the predicate p to c then return
it. If the file does not exist the we just return False.

> contents      :: (String -> Bool) -> FilePath -> IO Bool
> contents p f  = do b <- doesFileExist f
>                    if b then do c <- readFile f
>                                 return (p c)
>                         else return False

test result for this function:

*HW7> :!ls
HW1CS457.txt	HW2CS457.txt	HW3.hs		HW4.hs		HW4.lhs		HW6.lhs		HW7.lhs		lines.txt

*HW7> find "." -| contents (even . length . lines) >>= mapM_ putStrLn
./HW1CS457.txt
./HW2CS457.txt
./HW4.hs
./HW6.lhs
./lines.txt

Verify one short text file content:

*HW7> find "." -| size (<100) -|contents (even . length . lines) >>= mapM_ putStrLn
./lines.txt

the content of lines.txt has 4 lines:
1
2
3
4

--------------------------------------------------------------------------------

> display  :: FilePath -> IO Bool
> display f = do putStrLn f
>                return True

QUESTION 6: Define a filter command of the following type that allows for user 
interation in a find command:

This function looks like an imperative code and we need to print out the String
which is the path of a file fed to this queryUser included " (y/n)?" string.
Since we need to return IO Bool and we have yes or no answer, the function will 
simply return True or False after an if statement. 

One problem is that if we use getChar then the result looks like this:

*HW7> find "." -| haskellFiles -| size (>100) -| queryUser -| display
./HW3.hs (y/n)?
y./HW4.hs (y/n)?
n./HW4.lhs (y/n)?
y./HW6.lhs (y/n)?
y./HW7.lhs (y/n)?
n./HW3.hs
./HW4.lhs
./HW6.lhs  

so I choose to use getLine instead which will get a string to c. Then I add a line
let t = head c to get the first character in the line.

> queryUser  :: String -> IO Bool
> queryUser s = do 
>                  putStrLn $ s ++ " (y/n)?"
>                  do c <- getLine
>                     let t = head c
>                     if t == 'y' then return True
>                                 else return False					

The the result is :

*HW7> find "." -| haskellFiles -| size (>100) -| queryUser -| display

./HW3.hs (y/n)?
y
./HW4.hs (y/n)?
y
./HW4.lhs (y/n)?
n
./HW6.lhs (y/n)?
n
./HW7.lhs (y/n)?
n
./HW3.hs
./HW4.hs
["./HW3.hs","./HW4.hs"]

Test all n answer:
*> find "." -| haskellFiles -| size (>100) -| queryUser -| display
./HW3.hs (y/n)?
n
./HW4.hs (y/n)?
n
./HW4.lhs (y/n)?
n
./HW6.lhs (y/n)?
n
./HW7.lhs (y/n)?
n
[]

All y answer:
*HW7> find "." -| haskellFiles -| size (>100) -| queryUser -| display
./HW3.hs (y/n)?
yes
./HW4.hs (y/n)?
y
./HW4.lhs (y/n)?
y
./HW6.lhs (y/n)?
y
./HW7.lhs (y/n)?
y
./HW3.hs
./HW4.hs
./HW4.lhs
./HW6.lhs
./HW7.lhs
["./HW3.hs","./HW4.hs","./HW4.lhs","./HW6.lhs","./HW7.lhs"]


