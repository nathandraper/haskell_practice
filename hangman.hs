maxtries :: Int
maxtries = 5

main :: IO ()
main = do
  word <- getLine
  play maxtries word $ concat $ replicate (length word) "-"

play :: Int -> String -> String -> IO ()
play tries word guessword = do
  putStrLn guessword
  if not $ elem '-' guessword
    then do
      putStrLn "Correct! You win!"
      return ()
    else if (tries == 0)
      then do
        putStrLn "Out of guesses! You Lose!"
        return ()
      else do
        putStrLn "Tries left: "
        putStrLn $ show tries
        guess <- getChar
        putStrLn "\n"
        play (setTries word guess tries) word $ reveal word guessword guess

reveal :: String -> String -> Char -> String
reveal word guessword guess = map takeletter $ zip word guessword
  where takeletter (w, g) = if guess == w then w else g

setTries :: String -> Char -> Int -> Int
setTries word guess tries | elem guess word = tries
                          | otherwise       = tries - 1
