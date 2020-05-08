{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystem
  (
    CurrentDir (..),
    FileSystem,
    FSError (..),
    FileData (..),
    Directory (..),
    Directories (..),
    FilesInDir (..),
    File (..),
    SubDirs (..),
    ParentDir (..),
    Type (..),
    Name (..),
    searchDir,
    ModificationTime (..),
    Size (..),
    FileInfo (..),
    CountFiles (..),
    DirInfo (..),
    readFileSystem,
    getTime,
    getPathFromDirectory,
--    cd,
--    dir,
--    information,
--    searchFile,
    writeFileSystem
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (State, get)
import Data.List.Split (splitOn)
import System.IO (FilePath, readFile)
--import Control.Exception
--import Control.Applicative ((<|>))
--import Control.Monad (Monad)
import System.Directory (Permissions (..),
                         getCurrentDirectory, getFileSize, getPermissions,
                         listDirectory, doesDirectoryExist,
                         getModificationTime)
import Data.Time.Clock (UTCTime (..))

data FSError = ReadFile FilePath | WriteFile FilePath
              | CanNotFindFile FilePath

instance Show FSError where
  show (CanNotFindFile s) = "Can't find file: " ++ s
  show (ReadFile s) = "Can't read file: " ++ s
  show (WriteFile s) = "Can't write to file: " ++ s

newtype FileData = FileData (String)

data Type = NoExecutable | Executable

newtype ModificationTime = ModificationTime UTCTime

-- | Size of file or directory in bits
newtype Size = Size Integer

data FileInfo = FileInfo FilePath Permissions Type ModificationTime Size

newtype CountFiles = CountFiles Int

data DirInfo = DirInfo FilePath Size CountFiles Permissions

newtype File = File (FileInfo, FileData)

newtype FilesInDir = FilesInDir [File]

-- | Index of SubDirs in Tree
newtype SubDirs = SubDirs [Int]

-- | Index of Parent in Tree
newtype ParentDir = ParentDir Int

data Directory = Directory DirInfo FilesInDir

newtype CurrentDir = CurrentDir FilePath

data Directories = Directories [Directory] CurrentDir

type FileSystem e a = ExceptT e (State Directories) a

getDirInfo :: FilePath -> IO DirInfo
getDirInfo fp = do
  size <- getDirSize fp
  filesPath <- getFilesPath fp
  permissions <- getPermissions fp
  return $ DirInfo fp size (CountFiles $ length filesPath) permissions

getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
  permission <- getPermissions fp
  let fileType = getType permission
  time <- getTime fp
  size <- getFileSize fp
  return $ FileInfo fp permission fileType time (Size size)

getFileData :: FilePath -> IO FileData
getFileData fp = do
  data_ <- readFile fp
  return $ FileData data_

getTime :: FilePath -> IO ModificationTime
getTime fp = do
  time <- getModificationTime fp
  return $ ModificationTime time

getType :: Permissions -> Type
getType p
  | executable p = Executable
  | otherwise    = NoExecutable

getFileSizeFromFile :: [FilePath] -> Integer -> IO Integer
getFileSizeFromFile [] sum_ = return sum_
getFileSizeFromFile (fp : others) sum_ = do
  s <- getFileSize fp
  othersSum <- getFileSizeFromFile others (s + sum_)
  return $ othersSum

getDirSize :: FilePath -> IO Size
getDirSize fp = do
  filesPath <- getFilesPath fp
  dirSize <- getFileSizeFromFile filesPath 0
  return $ Size dirSize

-- | Returns file system
readFileSystem :: IO Directories
readFileSystem = do
  curDir <- getCurrentDirectory
  fs <- createFSDirectories (curDir : [])
  return $ Directories fs (CurrentDir curDir)

createFSDirectories :: [FilePath] -> IO [Directory]
createFSDirectories [] = return []
createFSDirectories (dirPath : others) = do
  filesPathInDir <- getFilesPath dirPath
  filesInDir <- getFilesFromList filesPathInDir
  dirInfo <- getDirInfo dirPath
  subDirsPath <- getSubDirsPath dirPath
  subDirs <- createFSDirectories subDirsPath
  othersDirs <- createFSDirectories others
  return $ [(Directory dirInfo (FilesInDir filesInDir))]
           ++ subDirs
           ++ othersDirs

getDirsFromList :: [FilePath] -> IO [FilePath]
getDirsFromList [] = return []
getDirsFromList (x : xs) = do
  isDir <- doesDirectoryExist x
  tail_ <- getDirsFromList xs
  if isDir
  then return $ x : tail_
  else return tail_

getSubDirsPath :: FilePath -> IO [FilePath]
getSubDirsPath fp = do
  list <- listDirectory fp
  dirs <- getDirsFromList list
  return dirs

-- | Gives only files without dirs
getFilesFromList :: [FilePath] -> IO [File]
getFilesFromList [] = return []
getFilesFromList (x : xs) = do
  fileInfo <- getFileInfo x
  fileData <- getFileData x
  xsFiles <- getFilesFromList xs
  return $ [(File (fileInfo, fileData))] ++ xsFiles

getFilesPathFromList :: [FilePath] -> IO [FilePath]
getFilesPathFromList []       = return []
getFilesPathFromList (x : xs) = do
  isDir <- doesDirectoryExist x
  tail_ <- getFilesPathFromList xs
  if isDir
  then return tail_
  else return $ x : tail_

-- | Returns only files without directories
getFilesPath :: FilePath -> IO [FilePath]
getFilesPath fp = do
  list <- listDirectory fp
  files <- getFilesPathFromList list
  return files

data Name = NameFile String | NameFolder String

--cd :: String -> FileSystem [Name]
--cd _ = undefined

-- | Search directory by name
searchDir :: String -> FileSystem FSError Directory
searchDir name = do
  directories <- get
  case findDirInList name directories of
    Nothing -> throwError $ CanNotFindFile name
    Just d  -> return d

findDirInList :: String -> Directories -> Maybe Directory
findDirInList _ (Directories [] _)        = Nothing
findDirInList name (Directories (x : xs) c)
  | getNameFromPathD x == name            = Just x
  | otherwise                             = findDirInList name
                                              (Directories xs c)

getPathFromDirectory :: Directory -> FilePath
getPathFromDirectory (Directory (DirInfo path _ _ _ ) _) = path


getNameFromPath :: FilePath -> String
getNameFromPath fp = last (splitOn "/" fp)

getNameFromPathD :: Directory -> String
getNameFromPathD (Directory (DirInfo path _ _ _) _) = getNameFromPath path

--searchFile :: String -> FileSystem File
--searchFile name = undefined

--dir :: FileSystem [Name]
--dir = do
--  curDir <- searchCurDir

--data Info = InfoForFile FileInfo | InfoForFir DirInfo

--dirInformation :: String -> FileSystem Info
--dirInformation name = undefined

--fileInformation :: String -> FileSystem Info
--fileInformation name = undefined

--information :: String -> FileSystem Info
--information name = (dirInformation name) <|> (fileInformation name)
--information name = undefined

writeFileSystem :: Directories -> IO ()
writeFileSystem _ = do
  putStrLn "!!!"

