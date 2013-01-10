Title: Fast Sudoku solver.
Team: Dev Desai (Original code by Prof. Chung Chieh Shan)
How to use: eg. Type "solve test5" without the quotes. There are seven test cases. 

Why we care about this problem?
We all saw in class that test2 required a long time to solve due to combinatorial
explosion. I had solved a few sudoku puzzles before and therefore wondered if 
some of the strategies I used could be implemented, so as to reduce backtracking.

How we did it?
As it turns out, the addition of just one strategy makes this solver churn out answers 
instantly for most of the real-world puzzles out there (PS: This could very well be an
optimistic extrapolation!). The strategy is usually called "Hidden Single" and is
explained as follows:

             "If a cell is the only one in a row, column or block that  
             can take a particular value, then it must have that value."

This program is an extension of the sudoku solver that the professor wrote in class 
and as such much of the explanation was written by him. 
Apart from tweaking a few functions, we add the following new functions:

1. pruneStar (iterative application of prune)
2. hiddenSingle (the strategy) and hS
3. pruneHiddenSingle (an example of constraint propagation)
4. pruneHiddenSingleStar (iterative application of pruneHiddenSingle)
5. pruneRows, pruneCols, pruneBoxs
6. solve


> module MySudoku where
> import List
> import Char

We represent a board as a list of rows.  Each row is in turn a list of cells.
This representation is by far not the only viable one.

> type Board a = [[a]]

> rows :: Board a -> [[a]]
> rows = id

To turn a board into a list of columns, we transpose the board as a list of
lists.

> cols :: Board a -> [[a]]
> cols [] = []
> cols ([]:_) = []
> cols xss = map head xss : cols (map tail xss)

To turn the board into a list of boxes, we treat the board as a
four-dimensional array and transpose the middle two dimensions.

> boxs :: Board a -> [[a]]
> boxs = map concat . concat . map cols . chop' . map chop'

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)

> chop' :: [a] -> [[a]]
> chop' xs = chop n xs
>     where n = round (sqrt (fromIntegral (length xs)))

It is useful below for rows, cols, and boxs to each be their own inverse.

A solution is a board without any conflict. 
The following function is slightly tweaked from before.

> conflict :: Board [Int] -> Bool
> conflict b = any dupes (rows b)
>           || any dupes (cols b)
>           || any dupes (boxs b)

A conflict occurs when two cells in the same row, column, or box are the same. 
The following function is slightly tweaked from before.

> dupes :: [[Int]] -> Bool
> dupes [] = False
> dupes (x:xs) = x `elem` xs || dupes xs

We represent an incomplete board as a board whose cells are each a list of
possibilities.  For example, in a standard 9x9 Sudoku game, a cell containing
[3] is known to be 3, whereas a cell containing [1..9] is completely unknown.
A cell containing [] indicates a dead end for the search; we consider such a
cell unknown rather than known.

(An alternative representation of incomplete boards is to represent only cells
that are completely unknown (say with the number 0, or Nothing) or completely
known.  In that case, we would expand the search by selecting the "most known"
cell among the unknown ones and exploring all choices for that cell.)

> known :: [a] -> Bool
> known [_] = True
> known _ = False

> unknown :: [a] -> Bool
> unknown = not . known

The prune function uses the known cells in a board of possibilities to reduce
the number of possibilities in the cells.  It uses the eliminate function to do
the real job. "eliminate" is essentially the simplest strategy 
(also known as Naked Single) for pruning the set of choices.

> prune :: Board [Int] -> Board [Int]
> prune = rows . map eliminate . rows
>       . cols . map eliminate . cols
>       . boxs . map eliminate . boxs 

> eliminate :: [[Int]] -> [[Int]]
> eliminate cells = map elim cells
>     where knowns = concat (filter known cells)
>           elim xs = filter (not . (`elem` (if known xs then knowns \\ xs
>                                                        else knowns      ))) xs
> --        elim xs = if known xs then filter (not . (`elem` (knowns \\ xs))) xs
> --                              else filter (not . (`elem`  knowns       )) xs

The addNumber function maps a board that is not entirely known to a list of
ways to expand that board into a more known board.  It always expands the first
unknown cell in the list of lists of cells.

> addNumber :: Board [Int] -> [Board [Int]]
> addNumber b = [ above ++ (left ++ [choice]:right) : below | choice <- cell ]
>     where (above, row:below) = break (any unknown) b
>           (left, cell:right) = break unknown row

The expand function searches for a solution by pruning a given board and
expanding it into a more known board if the result of pruning is not completely
known already.

> expand :: Board [Int] -> [Board [Int]]
> expand b | all (all known) b' = [b']
>          | otherwise          = concat (map expand (addNumber b'))
>     where b' = pruneHiddenSingleStar b


Here is the pruneStar function.

> pruneStar :: Board [Int] -> Board [Int]
> pruneStar a = if (length (concat (concat a)) == length (concat (concat b))) then a 
>               else pruneStar b 
>     where b = prune a

> pruneHiddenSingleStar :: Board [Int] -> Board [Int]
> pruneHiddenSingleStar a = if (length (concat (concat a)) == length (concat (concat b))) then a 
>                           else pruneHiddenSingleStar b 
>     where b = pruneHiddenSingle a


Here is the hiddenSingle strategy. pruneStar needs to be reapplied after each block prune 
so as to remove inconsistencies in the lists of possible candidates

> pruneHiddenSingle :: Board [Int] -> Board [Int]
> pruneHiddenSingle b = pruneStar (pruneRows (pruneStar (pruneCols 
>                         (pruneStar (pruneBoxs (pruneStar b)))))) 

> pruneRows :: Board [Int] -> Board [Int]
> pruneRows = rows . map hiddenSingle . rows

> pruneCols :: Board [Int] -> Board [Int]
> pruneCols = cols . map hiddenSingle . cols

> pruneBoxs :: Board [Int] -> Board [Int]
> pruneBoxs = boxs . map hiddenSingle . boxs


> hiddenSingle :: [[Int]] -> [[Int]]
> hiddenSingle cells = hS (concat (filter unknown cells)) cells

> hS :: [Int] -> [[Int]] -> [[Int]]
> hS _ [] = [] 
> hS unknowns (cell:cells) = if (length cell <= 1 || cell' == []) then (cell:hS unknowns cells) 
>                            else (cell':hS (unknowns \\ cell) cells)
>           where cell' = cell \\ (unknowns \\ cell) 


Seven boards of characters, for testing. The first two were given by the professor. 
Tests 3, 4, 5 are "easy" sudoku puzzles from Targum. Test 6 is a "hard" puzzle in Targum.
Test 7 was classified as "insanely hard" by one sudoku website.

> test1, test2, test3, test4, test5, test6, test7 :: Board Char
> test1 = ["1..9.7..3",
>          ".8.....7.",
>          "..9...6..",
>          "..72.94..",
>          "41.....95",
>          "..85.43..",
>          "..3...7..",
>          ".5.....4.",
>          "2..8.6..9"]
> test2 = [".98......",
>          "....7....",
>          "....15...",
>          "1........",
>          "...2....9",
>          "...9.6.82",
>          ".......3.",
>          "5.1......",
>          "...4...2."]
> test3 = ["..35..8.1",
>          ".54..8.2.",
>          ".7.6.....",
>          "8......9.",
>          "41.....76",
>          ".6......8",
>          ".....9.4.",
>          ".2.4..31.",
>          "3.9..52.."]
> test4 = ["..9.8...2",
>          "3..61....",
>          "71..39...",
>          "4.3.....8",
>          "92.....14",
>          "6.....5.3",
>          "...26..85",
>          "....91..7",
>          "8...4.3.."]
> test5 = ["...1.6...",
>          "96.....34",
>          ".8..2..1.",
>          "37.6.4.21",
>          "....8....",
>          "42.5.3.79",
>          ".9..3..6.",
>          "21.....95",
>          "...9.7..."]
> test6 = ["....1869.",
>          "..57.....",
>          ".9...2...",
>          "4.61...3.",
>          "...2.6...",
>          ".8...97.2",
>          "...8...7.",
>          ".....75..",
>          ".5342...."]
> test7 = [".......6.",
>          "28......4",
>          "..7..58..",
>          "5..34..2.",
>          "4..5.1..8",
>          ".1..76..3",
>          "..51..2..",
>          "3......81",
>          ".9......."]


Convert a board of characters, which is nice to type in and look at, to a board
of possibilities, which can be fed to the expand function as input.

> convert :: Board Char -> Board [Int]
> convert = map (map f)
>     where f '.' = [1..9]
>           f c | isDigit c = [read [c]]

This function is just because we are lazy.

> solve :: Board Char -> [Board [Int]]
> solve t = expand (convert t)



