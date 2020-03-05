import           Text.Read                      ( readMaybe )
import           System.IO                      ( stdin
                                                , hSetEcho
                                                )

type Board = [Int]

putStr' :: String -> IO ()
putStr' s = sequence_ [ putChar c | c <- s ]

putStrLn' :: String -> IO ()
putStrLn' s = putStr' $ s ++ "\n"

putboard :: Board -> IO ()
putboard []    = return ()
putboard board = do
  putStrLn $ "|" ++ replicate (head board) '*'
  putboard $ drop 1 board

putboard' :: Board -> IO ()
putboard' board =
  sequence_ [ putStrLn $ "|" ++ replicate row '*' | row <- board ]

adder :: IO ()
adder = do
  putStrLn "How many numbers? "
  total <- getInt
  s     <- getnums total
  putStrLn $ "The total is: " ++ show s

getnums :: Int -> IO Int
getnums 0     = return 0
getnums total = do
  num     <- getLine
  nextnum <- getnums (total - 1)
  return $ (read num :: Int) + nextnum

adder' :: IO ()
adder' = do
  putStrLn "How many numbers? "
  total <- getInt
  res   <- sequence $ concat $ replicate total [getInt]
  putStrLn $ "The total is: " ++ (show $ sum $ res)

readLine :: IO String
readLine = do
  ch <- getCh
  if ch == '\DEL'
    then do
      putChar '\b'
      putChar ' '
      putChar '\b'
      return [ch]
    else if ch == '\n'
      then do
        putChar ch
        return ""
      else do
        putChar ch
        str <- readLine
        if str == "\DEL" then readLine else return (ch : str)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

getInt :: IO Int
getInt = do
  putStrLn "Enter an Integer: "
  n <- getLine
  if (readMaybe n :: Maybe Int) == Nothing
    then do
      putStrLn "Error: not an integer.\n"
      getInt
    else return (read n :: Int)


