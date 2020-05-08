module Main
  (
    main
  ) where

import FileSystem (Directories (..), readFileSystem, writeFileSystem, searchDir,
                   getPathFromDirectory)
import Parser
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import System.IO (hFlush, stdout)
--import Data.Functor.Identity (runIdentity)

main :: IO ()
main = do
  fs <- readFileSystem
  newFS <- doCommands fs
  writeFileSystem newFS

doCommands :: Directories -> IO (Directories)
doCommands fsDirs = do
  putStr "> " >> hFlush stdout
  line <- getLine
  let addToInfo = fullDesc <> progDesc "File System program"
                  <> header "Homework 2"
  let parsed = execParserPure defaultPrefs
               (info (parserCommand <**> helper) addToInfo) (words line)
  case parsed of
    Failure e                    -> do
      putStrLn $ "\n" ++ show e
      doCommands fsDirs
    Success (Cd name)            -> do
      putStrLn $ "!cd " ++ name
      doCommands fsDirs
    Success Dir                  -> do
--      let _ = (runStateT (runFileSystem $ dir) fsDirs)
--                      `catchError` hendler
--      doCommands fsDirs
      putStrLn $ "!Dir"
      doCommands fsDirs
    Success (CreateFolder name)  -> do
      putStrLn $ "!CF" ++ name
      doCommands fsDirs
    Success (Cat name)           -> do
      putStrLn $ "!Cat" ++ name
      doCommands fsDirs
    Success (CreateFile name)    -> do
      putStrLn $ "!CF" ++ name
      doCommands fsDirs
    Success (Remove name)        -> do
      putStrLn $ "!Rm" ++ name
      doCommands fsDirs
    Success (WriteToFile name w) -> do
      putStrLn $ "!WTF " ++ name ++ " text: " ++ w
      doCommands fsDirs
    Success (FindFile name)      -> do
      putStrLn $ "!Find" ++ name
      doCommands fsDirs
    Success (FindDir name)       -> do
      let (res, _) = runState (runExceptT (searchDir name)) fsDirs
      case res of
        Left e -> putStrLn $ show e
        Right d -> putStrLn $ "Path: " ++ (getPathFromDirectory d)
      doCommands fsDirs
    Success (Information name)   -> do
      putStrLn $ "!Info" ++ name
      doCommands fsDirs
    Success Quit                 -> do
      putStrLn $ "!Quit"
      return fsDirs
    CompletionInvoked a          -> do
      putStrLn $ "Can't parse! ++" ++ show a
      doCommands fsDirs

--hendler :: FSError -> Except FSError a
--hendler = undefined


