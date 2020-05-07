module Main
  (
    main
  ) where

--import FileSystem (FileSystem (..))
import Parser
import Options.Applicative
import Data.Semigroup ((<>))

--main :: IO ()
--main = greet =<< execParser opts
--  where
--    opts = info (sample <**> helper)
--      ( fullDesc
--     <> progDesc "Print a greeting for TARGET"
--     <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  putStr "> "
  line <- getLine
  let addToInfo = fullDesc <> progDesc "File System program"
                  <> header "Homework 2"
  let parsed = execParserPure defaultPrefs
               (info (parserCommand <**> helper) addToInfo) (words line)
  case parsed of
    Failure e                    -> do
      putStrLn $ "\n" ++ show e
      main
    Success (Cd name)            -> do
      putStrLn $ "!Cd " ++ name
      main
    Success Dir                  -> do
      putStrLn $ "!Dir"
      main
    Success (CreateFolder name)  -> do
      putStrLn $ "!CF" ++ name
      main
    Success (Cat name)           -> do
      putStrLn $ "!Cat" ++ name
      main
    Success (CreateFile name)    -> do
      putStrLn $ "!CF" ++ name
      main
    Success (Remove name)        -> do
      putStrLn $ "!Rm" ++ name
      main
    Success (WriteToFile name w) -> do
      putStrLn $ "!WTF " ++ name ++ " text: " ++ w
      main
    Success (FindFile name)      -> do
      putStrLn $ "!Find" ++ name
      main
    Success (Information name)   -> do
      putStrLn $ "!Info" ++ name
      main
    Success Quit                 -> putStrLn $ "!Quit"
    CompletionInvoked a          -> do
      putStrLn $ "Can't parse! ++" ++ show a
      main

