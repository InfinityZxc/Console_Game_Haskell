import System.IO
import System.Random
import System.Console.ANSI

newRow :: RandomGen g => g -> [Int]
newRow gen = take 16 (randomRs  (2, 9) gen :: [Int])

createField :: [[Int]]
createField = (replicate 10 $ replicate 16 0) ++ [[0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0]]

printCellTop :: Int -> IO ()
printCellTop num = do
    let picture = case num of
            1           -> "  o   "
            2           -> "  /\\  " -- tree
            3           -> "  _   "  -- duck
            4           -> "      "  -- bat
            5           -> "  |   "  -- cactus
            6           -> " ()-()"  -- mice
            7           -> "W ()  "  -- devil
            otherwise   -> "      "
    setSGR [SetColor Foreground Vivid White]
    putStr "|"
    case num of
            1           -> setSGR [SetColor Background Vivid White, SetColor Foreground Dull Cyan]
            2           -> setSGR [SetColor Foreground Vivid Green]
            3           -> setSGR [SetColor Foreground Vivid Yellow]
            4           -> setSGR [SetColor Foreground Vivid Blue]
            5           -> setSGR [SetColor Foreground Dull Green]
            6           -> setSGR [SetColor Foreground Dull White]
            7           -> setSGR [SetColor Foreground Vivid Red]
            otherwise   -> setSGR [SetColor Foreground Vivid White]
    putStr picture
    setSGR [SetColor Background Dull Black]

printCellMid :: Int -> IO ()
printCellMid num = do
    let picture = case num of
            1           -> " /[]\\ "
            2           -> " /  \\ "
            3           -> ">(.)__"
            4           -> " /^v^\\"
            5           -> "(_|_) " 
            6           -> "  \\\"/ "
            7           -> "|-><  "
            otherwise   -> "      "
    setSGR [SetColor Foreground Vivid White]
    putStr "|"
    case num of
            1           -> setSGR [SetColor Background Vivid White, SetColor Foreground Dull Cyan]
            2           -> setSGR [SetColor Foreground Vivid Green]
            3           -> setSGR [SetColor Foreground Vivid Yellow]
            4           -> setSGR [SetColor Foreground Vivid Blue]
            5           -> setSGR [SetColor Foreground Dull Green]
            6           -> setSGR [SetColor Foreground Dull White]
            7           -> setSGR [SetColor Foreground Vivid Red]
            otherwise   -> setSGR [SetColor Foreground Vivid White]
    putStr picture
    setSGR [SetColor Background Dull Black]

printCellBot :: Int -> IO ()
printCellBot num = do
    let picture = case num of
            1           -> "  /\\  "
            2           -> " \\  / "
            3           -> " (___/"
            4           -> "      "
            5           -> "  |   "
            6           -> "   `  "
            7           -> "| )(\\_"
            otherwise   -> "      "
    setSGR [SetColor Foreground Vivid White]
    putStr "|"
    case num of
            1           -> setSGR [SetColor Background Vivid White, SetColor Foreground Dull Cyan]
            2           -> setSGR [SetColor Foreground Vivid Green]
            3           -> setSGR [SetColor Foreground Vivid Yellow]
            4           -> setSGR [SetColor Foreground Vivid Blue]
            5           -> setSGR [SetColor Foreground Dull Green]
            6           -> setSGR [SetColor Foreground Dull White]
            7           -> setSGR [SetColor Foreground Vivid Red]
            otherwise   -> setSGR [SetColor Foreground Vivid White]
    putStr picture
    setSGR [SetColor Background Dull Black]

printRow :: [Int] -> IO ()
printRow row = do 
    putStrLn (replicate 113 '-')
    mapM_ printCellTop row
    setSGR [SetColor Foreground Vivid White]
    putStr "|\n"
    mapM_ printCellMid row
    setSGR [SetColor Foreground Vivid White]
    putStr "|\n"
    mapM_ printCellBot row
    setSGR [SetColor Foreground Vivid White]
    putStr "|\n"

printField :: [[Int]] -> IO ()
printField field = do
    mapM_ printRow field
    setSGR [SetColor Foreground Vivid White]
    putStrLn (replicate 113 '-')

combineRows :: [Int] -> [Int] -> String -> [Int]
combineRows (x1:x2:x3:xs) (y1:y2:y3:ys) dir = 
    case dir of
        "left"      -> if y2 == 1 || y1 == 1 then 1 : combineRows (x2:x3:xs) (0:y3:ys) "none" else x1 : combineRows (x2:x3:xs) (y2:y3:ys) "left"
        "right"     -> if y1 == 1 then x1 : combineRows (x2:x3:xs) (1:y3:ys) "none" else x1 : combineRows (x2:x3:xs) (y2:y3:ys) "right"
        "none"      -> if y1 == 1 then 1 : combineRows (x2:x3:xs) (y2:y3:ys) "none" else x1 : combineRows (x2:x3:xs) (y2:y3:ys) "none"
combineRows (x1:x2:[]) (y1:y2:[]) dir = 
    case dir of
        "left"      -> if y2 == 1 then 1 : x2 : [] else x1 : x2 : []
        "right"     -> if y1 == 1 || y2 == 1 then x1 : 1 : [] else x1 : x2 : []
        "none"      -> if y1 == 1 then 1 : x2 : [] else if y2 == 1 then x1 : 1 : [] else x1 : x2 : []
combineRows x y dir = x

findObject :: [Int] -> [Int] -> String -> Int
findObject (x1:x2:x3:xs) (y1:y2:y3:ys) dir = 
    case dir of
        "left"      -> if y2 == 1 || y1 == 1 then x1 else findObject (x2:x3:xs) (y2:y3:ys) "left"
        "right"     -> if y1 == 1 then x2 else findObject (x2:x3:xs) (y2:y3:ys) "right"
        "none"      -> if y1 == 1 then x1 else findObject (x2:x3:xs) (y2:y3:ys) "none"
findObject (x1:x2:[]) (y1:y2:[]) dir = 
    case dir of
        "left"      -> if y2 == 1 then x1 else 0
        "right"     -> if y1 == 1 || y2 == 1 then x2 else 0
        "none"      -> if y1 == 1 then x1 else if y2 == 1 then x2 else 0
findObject x y dir = 0

movePlayer :: [[Int]] -> String -> [[Int]]
movePlayer (x:y:[]) dir = (combineRows x y dir) : []
movePlayer (x:xs) dir = x : (movePlayer xs dir)

findWhatHappened :: [[Int]] -> String -> Int
findWhatHappened (x:y:[]) dir = (findObject x y dir)
findWhatHappened (x:xs) dir = (findWhatHappened xs dir)

doAction field dir = (movePlayer field dir, findWhatHappened field dir)

turn :: [[Int]] -> Int -> Int -> Int -> IO ()
turn field hp dif cnt = do
    gen <- newStdGen
    action <- getLine
    let direction = case action of
            "left"      -> "left"
            "l"         -> "left"
            "right"     -> "right"
            "r"         -> "right"
            otherwise   -> "none"
    let actionResult = doAction field direction
    let fieldNew = (newRow gen) : (fst actionResult)
    clearScreen
    let newHp = case snd actionResult of
            2           -> if dif == 5 then hp + 1 else hp + 5 - dif
            3           -> if dif == 4 then hp - 1 else hp + 4 - dif
            4           -> if dif == 3 then hp - 1 else hp + 3 - dif
            5           -> if dif == 2 then hp - 1 else hp + 2 - dif
            6           -> if dif == 1 then hp - 1 else hp + 1 - dif
            7           -> hp - dif
            otherwise   -> hp
    putStrLn $ "CURRENT HP : " ++ show newHp
    printField fieldNew
    if newHp > 0 then
        turn fieldNew newHp dif (cnt+1)
    else 
        putStrLn $ "Your score = " ++ show(cnt)

game :: IO ()
game = do
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Dull Black]
    putStrLn "Choose difficulty by typing 1 - 5"
    diffInput <- getLine
    let hp = case diffInput of
            "1"             -> 10
            "2"             -> 7
            "3"             -> 5
            "4"             -> 3
            "5"             -> 1
            otherwise       -> 5
    let dif = case diffInput of
            "1"             -> 1
            "2"             -> 2
            "3"             -> 3
            "4"             -> 4
            "5"             -> 5
            otherwise       -> 3
    let field = createField
    clearScreen
    printField field
    turn field hp dif 0
    putStrLn "Game Over"
    confirm <- getLine
    game

tutorial :: IO ()
tutorial = do
    putStrLn "Comands: \"right\" to move one tile to the right, \"left\" to move one tile to the left, anything else to stay in place."
    putStrLn "Enter anything to start"

main :: IO ()
main = do   
    clearScreen
    tutorial
    confirm <- getLine
    game