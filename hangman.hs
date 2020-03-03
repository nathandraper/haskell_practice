maxtries :: Int
maxtries = 5

hangman :: IO ()
hangman = do
  word <- getLine
  play maxtries word $ concat $ replicate (length word) "-"

play :: Int -> String -> String -> IO ()
play tries word guessword = do
  putStrLn guessword
  putStrLn word
  if not $ elem '-' guessword
    then do
      putStrLn "Correct! You win!"
      return ()
    else if (tries == 0)
      then do
        putStrLn "Out of guesses! You Lose!"
        return ()
      else do
        guess <- getChar
        print guess
        print guess
        print guess
        play (tries - 1) word $ reveal word guessword guess

reveal :: String -> String -> Char -> String
reveal word guessword guess = map takeletter $ zip word guessword
  where takeletter (w, g) = if guess == w then w else g
