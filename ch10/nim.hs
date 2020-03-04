import           Text.Read                      ( readMaybe )

data Player = One | Two deriving (Show)

type Board = [Int]
type Row = Int
type Stars = Int
type Move = (Row, Stars)

-- IO

main :: IO ()
main = do
  play initial One

play :: Board -> Player -> IO ()
play board player = do
  putboard board
  if gameover board
    then do
      putStrLn $ "Player " ++ (show $ next player) ++ " Wins!"
    else do
      putStrLn $ "Player " ++ (show player) ++ "'s Turn"
      putStrLn "Select Row: "
      row   <- getInt
      stars <- getInt
      if validmove board (row, stars)
        then play (makemove board (row, stars)) (next player)
        else do
          putStrLn "Invalid move."
          play board player

putboard :: Board -> IO ()
putboard []    = return ()
putboard board = do
  putStrLn $ "|" ++ replicate (head board) '*'
  putboard $ drop 1 board

getInt :: IO Int
getInt = do
  putStrLn "Enter an Integer: "
  n <- getLine
  if (readMaybe n :: Maybe Int) == Nothing
    then do
      putStrLn "Error: not an integer.\n"
      getInt
    else return (read n :: Int)

-- Pure

initial :: Board
initial = [5, 4, 3, 2, 1]

validmove :: Board -> Move -> Bool
validmove board (row, n) | board !! (row - 1) >= n = True
                         | otherwise               = False

makemove :: Board -> Move -> Board
makemove board (row, n) =
  take (row - 1) board ++ board !! (row - 1) - n : drop row board

gameover :: Board -> Bool
gameover board = foldr (+) 0 board == 0

next :: Player -> Player
next One = Two
next Two = One

