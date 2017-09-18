import Data.List.Split -- splitOn
import Data.List -- intersperse
import Debug.Trace -- trace
import System.Environment -- getArgs

type Path = [Char]
type RawSudoku = [[Int]]
type EnumeratedRawSudoku = [[(Int, Int, Int)]]
type Sudoku = [[Field]]
type XCoord = Int
type YCoord = Int
type Value = Int
data SudokuResult = WIP Sudoku | Finished Sudoku | Error String Sudoku
data Field = Todo Int Int Int | Done Int Int Int | Options Int Int [Int] deriving (Show, Eq)

fieldX :: Field -> Int
fieldX (Todo x _ _) = x
fieldX (Done x _ _) = x
fieldX (Options x _ _) = x

fieldY :: Field -> Int
fieldY (Todo _ y _) = y
fieldY (Done _ y _) = y
fieldY (Options _ y _) = y

-- PARSING

matchCharacter :: Char -> Int
matchCharacter '.' = 0
matchCharacter '1' = 1
matchCharacter '2' = 2
matchCharacter '3' = 3
matchCharacter '4' = 4
matchCharacter '5' = 5
matchCharacter '6' = 6
matchCharacter '7' = 7
matchCharacter '8' = 8
matchCharacter '9' = 9
matchCharacter c = error ("Invalid character: " ++ show c)

invalidFormatErr :: String
invalidFormatErr = "Invalid format: The input must be a 9x9 number matrix seperated by newline characters. Unknown fields are marked with '.'"

parseRaw :: String -> RawSudoku
parseRaw src = 
    let raw = map (\x -> map matchCharacter x) $ lines src
    in if (not $ all (\x -> length x == 9) raw) || length raw /= 9 then error invalidFormatErr else raw

enumerateRaw :: RawSudoku -> EnumeratedRawSudoku
enumerateRaw s = chunksOf 9 $ zipWith (\c v -> (fst c, snd c, v)) [(x, y) | y <- [0..8], x <- [0..8]] $ concat s

convFields :: EnumeratedRawSudoku -> Sudoku
convFields = map (\row -> map (\(x, y, f) -> if f == 0 then Options x y [1..9] else Todo x y f) row)

parseSudoku :: String -> Sudoku
parseSudoku = convFields . enumerateRaw . parseRaw

-- SOLVING

isTodo :: Field -> Bool
isTodo (Todo _ _ _) = True
isTodo _ = False

isOptions :: Field -> Bool
isOptions (Options _ _ _) = True
isOptions _ = False

listTodos :: Sudoku -> [Field]
listTodos = filter isTodo . concat

listOptions :: Sudoku -> [Field]
listOptions = filter isOptions . concat

firstOption :: Sudoku -> Field
firstOption = head . listOptions

eliminateField_Int :: Field -> Value -> Field
eliminateField_Int (Options x y values) v = 
    let newValues = filter (/=v) values
    in (Options x y newValues)
eliminateField_Int f _ = f

eliminateField :: XCoord -> YCoord -> Value -> Sudoku -> Sudoku
eliminateField x y v s = map (\row -> map (\f -> if fieldX f == x && fieldY f == y then eliminateField_Int f v else f) row) s

eliminateFields :: [(XCoord, YCoord)] -> Value -> Sudoku -> Sudoku
eliminateFields [] v s = s
eliminateFields (x:xs) v s = eliminateField (fst x) (snd x) v $ eliminateFields xs v s

eliminateRow :: YCoord -> Value -> Sudoku -> Sudoku
eliminateRow y v s = eliminateFields (map (\x -> (x, y)) [0..8]) v s

eliminateCol :: XCoord -> Value -> Sudoku -> Sudoku
eliminateCol x v s = eliminateFields (map (\y -> (x, y)) [0..8]) v s

eliminateSquare :: XCoord -> YCoord -> Value -> Sudoku -> Sudoku
eliminateSquare x y v s =
    let squareX = floor ((realToFrac x) / 3)
        squareY = floor ((realToFrac y) / 3)
    in eliminateFields [(x + squareX * 3, y + squareY * 3) | x <- [0..2], y <- [0..2]] v s

eliminateTodo :: Field -> Sudoku -> Sudoku
eliminateTodo (Todo x y v) s = eliminateSquare x y v $ eliminateCol x v $ eliminateRow y v s

markFieldAsDone :: Field -> Field
markFieldAsDone (Todo x y v) = (Done x y v)
markFieldAsDone f = f

processTodos :: Sudoku -> [Field] -> Sudoku
processTodos s (x:xs) =
    let previous = processTodos s xs
        markedDone = map (\row -> map markFieldAsDone row) previous
    in eliminateTodo x markedDone
processTodos s [] = s

markSolvedFieldsAsDone :: Field -> Field
markSolvedFieldsAsDone (Options x y v) = if length v == 1 then (Todo x y (head v)) else (Options x y v)
markSolvedFieldsAsDone f = f

markSolvedAsDone :: Sudoku -> Sudoku
markSolvedAsDone s = map (\row -> map markSolvedFieldsAsDone row) s

isSolvableField :: Field -> Bool
isSolvableField (Done _ _ _) = True
isSolvableField (Todo _ _ _) = True
isSolvableField (Options _ _ v) = length v /= 0

isSolvable :: Sudoku -> Bool
isSolvable = all (\row -> all isSolvableField row)

isFinishedField :: Field -> Bool
isFinishedField (Done _ _ _) = True
isFinishedField _ = False

isFinished :: Sudoku -> Bool
isFinished = all (\row -> all isFinishedField row)

solveAgainIfNotFinished :: SudokuResult -> SudokuResult
solveAgainIfNotFinished (WIP s) = if isFinished s then (Finished s) else solveSudoku s
solveAgainIfNotFinished v = v

substituteGuess :: XCoord -> YCoord -> Value -> Sudoku -> Sudoku
substituteGuess x y v = map (\row -> map (\f -> if fieldX f == x && fieldY f == y then (Todo x y v) else f) row)

guessFirstOption :: Sudoku -> [Sudoku]
guessFirstOption s =
    let (Options x y v) = firstOption s
    in zipWith (\a b -> substituteGuess x y a b) v $ take (length v) $ repeat s

isError :: SudokuResult -> Bool
isError (Error _ _) = True
isError _ = False

resolveAmbiguities :: SudokuResult -> Sudoku -> SudokuResult
resolveAmbiguities (WIP s) origin = 
    if s == origin 
    then 
        let guessingResults = filter (not . isError) $ map solveSudoku $ guessFirstOption s
        in 
            if length guessingResults == 0 
            then (Error "Each possible solution contains errors" s) 
            else head guessingResults
    else (WIP s)
resolveAmbiguities s _ = s

solveSudoku :: Sudoku -> SudokuResult
solveSudoku s = 
    let maybeCurrent = markSolvedAsDone $ processTodos s $ listTodos s
        currentMaybeAmbig = if isSolvable maybeCurrent then (WIP maybeCurrent) else (Error "The sudoku is not solvable" maybeCurrent)
        current = resolveAmbiguities currentMaybeAmbig s
        next = solveAgainIfNotFinished current
    in next

-- PRINTING

sudokuResultString :: SudokuResult -> String
sudokuResultString (Finished s) = "Solution:\n" ++ sudokuString s
sudokuResultString (WIP s) = "Computation not finished:\n" ++ sudokuString s
sudokuResultString (Error msg s) = msg ++ ":\n" ++ sudokuString s

sudokuString :: Sudoku -> String
sudokuString s = foldl (++) "" $ intersperse "\n" $ map lineString s

lineString :: [Field] -> String
lineString l = foldl (++) "" $ intersperse " " $ map fieldString l

fieldString :: Field -> String
fieldString (Todo _ _ v) = show v
fieldString (Done _ _ v) = show v
fieldString (Options _ _ v) = if length v == 0 then "E" else (if length v == 1 then show $ head v else "?")

-- USER INTERFACE

readAndSolve :: Path -> IO SudokuResult
readAndSolve path = do
    src <- readFile path
    return $ solveSudoku $ parseSudoku src

solveAndPrint :: Path -> IO ()
solveAndPrint path = do
    src <- readFile path
    putStrLn $ sudokuResultString $ solveSudoku $ parseSudoku src

main = do
    args <- getArgs
    if length args == 0
    then putStrLn "USAGE: sudoku [file]"
    else solveAndPrint $ head args