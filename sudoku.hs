import qualified Data.List as L

let example = [[0,0,0, 2,0,5, 0,0,0],
          [0,9,0, 0,0,0, 7,3,0],
          [0,0,2, 0,0,9, 0,6,0],
          [2,0,0, 0,0,0, 4,0,9],
          [0,0,0, 0,7,0, 0,0,0],
          [6,0,9, 0,0,0, 0,0,1],
          [0,8,0, 4,0,0, 1,0,0],
          [0,6,3, 0,0,0, 0,8,0],
          [0,0,0, 6,0,8, 0,0,0]]

let emptySudoku = [[ 0 | _ <- [1..9]  ] | _ <- [1..9]]

let sudokuWithPointElements :: (Num a, Eq a) => [((a,a), a)] -> [[a]]
    sudokuWithPointElements pointElements =
      foldr (\(p,e) sudoku -> replaceElementInMatrix p e sudoku)
          emptySudoku
          pointElements

let replaceElementInList :: (Num a, Eq a) => a -> b -> [b] -> [b]
    replaceElementInList 0 e (x:xs) = e : xs
    replaceElementInList idx e (x:xs) = x : replaceElementInList (idx-1) e xs

let replaceElementInMatrix :: (Num a, Eq a) => (a,a) -> b -> [[b]] -> [[b]]
    replaceElementInMatrix (0,c) e (x:xs) = (replaceElementInList c e x) : xs
    replaceElementInMatrix (r,c) e (x:xs) = x : replaceElementInMatrix ((r-1),c) e xs

sudokuWithPointElements [((x,x), 9) | x <- [0..8]]

let isLineSolved line = L.sort line == [1..9]

let sudokuRows sudoku = sudoku
    sudokuColumns sudoku = L.transpose sudoku
    sudokuSections sudoku = map (map (foldr1 (++)) ) $ -- join groups
      L.transpose $ map (listInChunks 3) $ L.transpose $ -- separate columns in chunks of 3
      map (listInChunks 3) sudoku -- separte rows in chunks of 3

let sudokuSectionForPoint (r,c) =  (div r 3, div c 3)

let isSudokuSolved sudoku = all isLineSolved  (rows ++ columns ++ sections)
      where rows = sudokuRows sudoku
            columns = sudokuColumns sudoku
            sections = foldr1 (++) $ sudokuSections sudoku

let elementsNotInList :: (Eq a) => [a] -> [a] -> [a]
    elementsNotInList = foldr L.delete

let freeSpaceForSudoku :: (Num b, Eq b) => [[b]] -> Maybe (Int,Int)
    freeSpaceForSudoku sudoku = nextFree (0,0)
      where
        isFree (r,c) = sudoku !! r !! c == 0
        nextFree p@(8,8) = if (isFree p) then Just p else Nothing
        nextFree p@(r,8) = if (isFree p) then Just p else nextFree (r+1,0)
        nextFree p@(r,c) = if (isFree p) then Just p else nextFree (r,c+1)

let availableNumbersForSudokuInPoint :: (Num b, Eq b) => (Int, Int) -> [[b]] -> [b]
    availableNumbersForSudokuInPoint p sudoku = elementsNotInList possibleNumbers numbersInPoint
      where numbersInPoint = (rowNumbers p) ++ (columnNumbers p) ++ (sectionNumbers section)
            rowNumbers (r,_) = sudokuRows sudoku !! r
            columnNumbers (_,c) = sudokuColumns sudoku !! c
            section = sudokuSectionForPoint p
            sectionNumbers (sr, sc) = sudokuSections sudoku !! sr !! sc
            possibleNumbers = [1,2,3,4,5,6,7,8,9]

let listInChunks n [] = []
    listInChunks n list = first : listInChunks n rest
      where (first, rest) = splitAt n list


let solveSudoku :: (Num b, Ord b) => [[b]] -> [[[b]]]
    solveSudoku sudoku
      | (isSudokuSolved sudoku) = [sudoku]
      | otherwise = case free of
          Nothing -> []
          Just p -> concatMap solveSudoku $
            map (\n -> replaceElementInMatrix p n sudoku) (availNums p)
      where free = freeSpaceForSudoku sudoku
            availNums = flip availableNumbersForSudokuInPoint $ sudoku

head $ solveSudoku example
