import           Control.Concurrent
import           Text.Read                      ( readMaybe )

type Pos = (Int, Int)
type Board = [Pos]

sec2micro :: Int
sec2micro = 1000000

width :: Int
width = 10

height :: Int
height = 10

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

stasis :: Board
stasis =
  [ (1 , 1)
  , (2 , 2)
  , (3 , 3)
  , (4 , 4)
  , (5 , 5)
  , (6 , 6)
  , (7 , 7)
  , (8 , 8)
  , (9 , 9)
  , (10, 10)
  ]

death :: Board
death = [(1, 1), (3, 3), (5, 5), (7, 7), (9, 9)]

flicker :: Board
flicker = [(4, 5), (5, 5), (6, 5)]

explode :: Board
explode = [(3, 4), (4, 4), (5, 4), (4, 6), (5, 6), (6, 6)]

main :: IO ()
main = do
  fps <- getInt
  play glider fps

play :: Board -> Int -> IO ()
play board fps = do
  cls
  showboard board
  threadDelay $ sec2micro `div` fps
  play (next board) fps

showboard :: Board -> IO ()
showboard board = do
  cls
  sequence_ [ writeat "0" p | p <- board ]
  reset

getInt :: IO Int
getInt = do
  putStrLn "Enter an Integer: "
  n <- getLine
  if (readMaybe n :: Maybe Int) == Nothing
    then do
      putStrLn "Error: not an integer.\n"
      getInt
    else return (read n :: Int)

writeat :: String -> Pos -> IO ()
writeat s pos = do
  goto pos
  putStr s

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

cls :: IO ()
cls = putStr "\ESC[2J"

-- for testing
reset :: IO ()
reset = goto (0, 100)

-- Pure

next :: Board -> Board
next board = survivors board ++ births board

empties :: Board -> Board
empties board =
  [ (x, y) | x <- [1 .. width], y <- [1 .. height], not $ elem (x, y) board ]

births :: Board -> Board
births board = [ pos | pos <- empties board, isborn pos board ]

isborn :: Pos -> Board -> Bool
isborn pos board = length adj == 3 where adj = adjacents pos board

survivors :: Board -> Board
survivors board = [ pos | pos <- board, survives pos board ]

survives :: Pos -> Board -> Bool
survives pos board = length adj == 2 || length adj == 3
  where adj = adjacents pos board

adjacents :: Pos -> Board -> [Pos]
adjacents (x, y) board =
  [ (x', y')
  | x' <- [x - 1, x, x + 1]
  , y' <- [y - 1, y, y + 1]
  , (elem (wrap (x', y') width height) board) && (not (x' == x && y' == y))
  ]

wrap :: Pos -> Int -> Int -> Pos
wrap (x, y) width height = ((choose x width), (choose y height))
 where
  choose val size | val < 1    = size
                  | val > size = 1
                  | otherwise  = val

