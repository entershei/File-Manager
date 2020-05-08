module Main
  (
    main
  ) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Data.Semigroup ((<>))
import Options.Applicative
import Parser (Command (..), parserCommand)
import System.IO (hFlush, stdout)

import FileSystem (cat, cd, createFolder, dir, getCurDir, getNameFromPath, getPathFromDirectory,
                   getPathFromFile, information, readFileSystem, searchDir, searchFile,
                   writeFileSystem)
import FileSystemTypes (Directories)

main :: IO ()
main = do
  fs <- readFileSystem
  newFS <- doCommands fs
  writeFileSystem newFS

doCommands :: Directories -> IO (Directories)
doCommands fsDirs = do
  let currentNameDir = getCurDir fsDirs
  putStr ((getNameFromPath currentNameDir) ++ "> ") >> hFlush stdout
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
      let (res, newFS) = runState (runExceptT $ cd name) fsDirs
      case res of
        Left e  -> do
          putStrLn $ show e
          doCommands fsDirs
        Right _ -> do
          putStrLn $ "Currect directory was changed"
          doCommands newFS
    Success Dir                  -> do
      let (res, _) = runState (runExceptT dir) fsDirs
      case res of
        Left e  -> putStrLn $ show e
        Right f -> putStrLn $ show f
      doCommands fsDirs
    Success (CreateFolder name)  -> do
      let (res, newFS) = runState (runExceptT (createFolder name)) fsDirs
      case res of
        Left e  -> do
          putStrLn $ show e
          doCommands fsDirs
        Right _ -> do
          putStrLn $ "Folder was created"
          doCommands newFS
    Success (Cat name)           -> do
      let (res, _) = runState (runExceptT $ cat name) fsDirs
      case res of
        Left e  -> putStrLn $ show e
        Right f -> putStrLn $ show f
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
      let (res, _) = runState (runExceptT (searchFile name)) fsDirs
      case res of
        Left e  -> putStrLn $ show e
        Right f -> putStrLn $ "Path: " ++ (getPathFromFile f)
      doCommands fsDirs
    Success (FindDir name)       -> do
      let (res, _) = runState (runExceptT (searchDir name)) fsDirs
      case res of
        Left e  -> putStrLn $ show e
        Right d -> putStrLn $ "Path: " ++ (getPathFromDirectory d)
      doCommands fsDirs
    Success (Information name)   -> do
      let (res, _) = runState (runExceptT (information name)) fsDirs
      case res of
        Left e      -> putStrLn $ show e
        Right info_ -> putStrLn $ show info_
      doCommands fsDirs
    Success Quit                 -> do
      putStrLn $ "!Quit"
      return fsDirs
    CompletionInvoked a          -> do
      putStrLn $ "Can't parse! ++" ++ show a
      doCommands fsDirs

