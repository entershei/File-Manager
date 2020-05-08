{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystem
  (
    cat,
    cd,
    createFolder,
    dir,
    getCurDir,
    getNameFromPath,
    getPathFromDirectory,
    getPathFromFile,
    information,
    readFileSystem,
    remove,
    searchDir,
    searchFile,
    writeFileSystem
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Data.List.Split (splitOn)
import System.Directory (Permissions (..), createDirectory, doesDirectoryExist, emptyPermissions,
                         getCurrentDirectory, getFileSize, getModificationTime, getPermissions,
                         listDirectory, removeDirectory, setOwnerReadable, setOwnerSearchable,
                         setOwnerWritable)
import System.IO (readFile)

import FileSystemTypes (CountFiles (..), CurrentDir (..), DirInfo (..), Directories (..),
                        Directory (..), FSError (..), File (..), FileData (..), FileInfo (..),
                        FileSystem, FilesInDir (..), Info (..), ModificationTime (..), Names (..),
                        RelaitiveName (..), Size (..), Type (..))

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

-- | Create file system
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
  relativePaths <- listDirectory fp
  let absolutePaths = getAbsolutePaths fp relativePaths
  dirs <- getDirsFromList absolutePaths
  return dirs

getAbsolutePaths :: FilePath -> [FilePath] -> [FilePath]
getAbsolutePaths _ [] = []
getAbsolutePaths add (x : xs)
  = (add ++ "/" ++ x) : (getAbsolutePaths add xs)

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

-- | Returns only files in the dir without directories
getFilesPath :: FilePath -> IO [FilePath]
getFilesPath fp = do
  relativePaths <- listDirectory fp
  let absolutePaths = getAbsolutePaths fp relativePaths
  files <- getFilesPathFromList absolutePaths
  return files

cd :: String -> FileSystem FSError ()
cd ".." = do
  directories <- get
  let curDirPath = getCurDir directories
  let parent = findParent curDirPath (getDirectories directories)
  case parent of
    Nothing -> throwError $ CanNotGoHigherThanRoot
    Just d  -> modify (changeCurDir d)
cd name = do
  directories <- get
  let curDirPath = getCurDir directories
  let subDirs = findSubDirs curDirPath (getDirectories directories)
  case findDirInList name directories of
    Nothing -> throwError $ CanNotFindDir name
    Just d  -> do
      let ok = findSamePath d subDirs
      case ok of
        True  -> modify (changeCurDir d)
        False -> throwError $ CanNotMakeCD name

findParent :: FilePath -> [Directory] -> Maybe Directory
findParent _ []          = Nothing
findParent name (x : xs)
  | isParentOf (splitOn "/" name) (splitOn "/" (getPathFromDirectory x))
              = Just x
  | otherwise = findParent name xs

-- | Returns True iff the second argument is parent of the first
-- [isParentOf child parent]
isParentOf :: [FilePath] -> [FilePath] -> Bool
isParentOf [] _ = False
isParentOf (_ : []) [] = True
isParentOf _ [] = False
isParentOf (c : cs) (p : ps)
  | c == p    = isParentOf cs ps
  | otherwise = False

findSamePath :: Directory -> [Directory] -> Bool
findSamePath _ [] = False
findSamePath d (x : xs)
  | getPathFromDirectory d == getPathFromDirectory x = True
  | otherwise                                        = findSamePath d xs

changeCurDir :: Directory -> Directories -> Directories
changeCurDir newD (Directories a _)
  = Directories a (CurrentDir (getPathFromDirectory newD))

-- | Search directory by name
searchDir :: String -> FileSystem FSError Directory
searchDir name = do
  directories <- get
  case findDirInList name directories of
    Nothing -> throwError $ CanNotFindDir name
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
getNameFromPathD d = getNameFromPath $ getPathFromDirectory d

getPathFromFile :: File -> String
getPathFromFile (File ((FileInfo path  _ _ _ _), _)) = path

getNameFromPathF :: File -> String
getNameFromPathF f = getNameFromPath $ getPathFromFile f

findFile :: String -> Directories -> Maybe File
findFile _ (Directories [] _) = Nothing
findFile name (Directories ((Directory _ (FilesInDir files)) : xs) c)
  = case findFileFromFiles name files of
    Nothing -> findFile name (Directories xs c)
    Just f  -> Just f

findFileFromFiles :: String ->[File] -> Maybe File
findFileFromFiles _ []          = Nothing
findFileFromFiles name (x : xs)
  | getNameFromPathF x == name = Just x
  | otherwise                  = findFileFromFiles name xs

searchFile :: String -> FileSystem FSError File
searchFile name = do
  directories <- get
  case findFile name directories of
    Nothing -> throwError $ CanNotFindFile name
    Just f  -> return f

getCurDir :: Directories -> FilePath
getCurDir (Directories _ (CurrentDir d)) = d

getFilesInDir :: Directory -> [File]
getFilesInDir (Directory _ (FilesInDir xs)) = xs

createFileNames :: [File] -> [RelaitiveName]
createFileNames []       = []
createFileNames (x : xs) = RelativeNameFile
                             (getNameFromPath $getPathFromFile x)
                               : (createFileNames xs)

getDirectories :: Directories -> [Directory]
getDirectories (Directories ds _) = ds

dir :: FileSystem FSError Names
dir = do
  directories <- get
  let curDirPath = getCurDir directories
  curDir <- searchDir (getNameFromPath curDirPath)
  let files = createFileNames $ getFilesInDir curDir
  let dirs = createDirectoryNames
              $ findSubDirs curDirPath (getDirectories directories)
  return $ Names (files ++ dirs)

createDirectoryNames :: [Directory] -> [RelaitiveName]
createDirectoryNames []       = []
createDirectoryNames (x : xs) = RelativeNameDirectory
                                (getNameFromPath $ getPathFromDirectory x)
                                  : (createDirectoryNames xs)

findSubDirs :: FilePath -> [Directory] -> [Directory]
findSubDirs _  []         = []
findSubDirs name (x : xs)
  | isSubDir name (getPathFromDirectory x) = x : (findSubDirs name xs)
  | otherwise                              = findSubDirs name xs

-- | Returns true iff second is subDir of first.
isSubDir :: FilePath -> FilePath -> Bool
isSubDir main d = oneMoreElemt (splitOn "/" main) (splitOn "/" d)

-- | Returns true iff the second has one more element
oneMoreElemt :: [FilePath] -> [FilePath] -> Bool
oneMoreElemt [] []       = False
oneMoreElemt [] (_ : []) = True
oneMoreElemt [] _        = False
oneMoreElemt (_ : _) []  = False
oneMoreElemt (x : xs) (y : ys)
  | x == y    = oneMoreElemt xs ys
  | otherwise = False

dirInformation :: String -> FileSystem FSError (Maybe Info)
dirInformation name = do
  directories <- get
  let requestDir = findDirInList name directories
  case requestDir of
    Nothing -> return Nothing
    Just d  -> return $ Just (InfoForDir $ getDirectoryInfo d)

getDirectoryInfo :: Directory -> DirInfo
getDirectoryInfo (Directory di _) = di

fileInformation :: String -> FileSystem FSError (Maybe Info)
fileInformation name = do
  directories <- get
  let requestFile = findFile name directories
  case requestFile of
    Nothing -> return Nothing
    Just f  -> return $ Just (InfoForFile $ getFileInforFromFile f)

getFileInforFromFile :: File -> FileInfo
getFileInforFromFile (File (fi, _)) = fi

getFileDataFromFile :: File -> FileData
getFileDataFromFile (File (_, fd)) = fd

information :: String -> FileSystem FSError Info
information name = do
  dirInfo <- dirInformation name
  case dirInfo of
    Nothing -> do
      fileInfo <- fileInformation name
      case fileInfo of
        Nothing -> throwError $ CanNotFindFleOrDir name
        Just fi -> return fi
    Just di -> return di

cat :: String -> FileSystem FSError FileData
cat name = do
  directories <- get
  let requestFile = findFile name directories
  case requestFile of
    Nothing -> throwError $ CanNotFindFile name
    Just f  -> return $ getFileDataFromFile f

createFolder :: String -> FileSystem FSError ()
createFolder name = do
  directories <- get
  let curDirPath = getCurDir directories
  let permission = setOwnerSearchable True (setOwnerWritable True
                     (setOwnerReadable True emptyPermissions))
  let newDirInfo = DirInfo (curDirPath ++ "/" ++ name)
                     (Size 0) (CountFiles 0) permission
  let newDir = Directory newDirInfo (FilesInDir [])
  modify $ addDir newDir

addDir :: Directory -> Directories -> Directories
addDir add (Directories ds curD) = Directories (add : ds) curD

removeFolder :: String -> FileSystem FSError ()
removeFolder name = do
  directories <- get
  let curDirPath = getCurDir directories
  modify $ removeDir (curDirPath ++ "/" ++ name)

removeDir :: FilePath -> Directories -> Directories
removeDir name (Directories ds c) = Directories (removeFromList ds) c
  where
    removeFromList :: [Directory] -> [Directory]
    removeFromList []               = []
    removeFromList (x : xs)
      | getPathFromDirectory x == name = removeFromList xs
      | otherwise                      = x : (removeFromList xs)

remove :: String -> FileSystem FSError ()
remove name = do
  removeFolder name

writeFileSystem :: Directories -> IO ()
writeFileSystem (Directories newDs _) = do
  oldDirectories <- readFileSystem
  let toAddDirs = findDirsToAdd (getDirectories oldDirectories) newDs
  addNewDirs toAddDirs
  let toRemoveDirs = findDirsToRemove (getDirectories oldDirectories) newDs
  removeOldDirs toRemoveDirs

removeOldDirs :: [FilePath] -> IO ()
removeOldDirs []       = return ()
removeOldDirs (x : xs) = do
  removeDirectory x
  removeOldDirs xs

-- | [old new]
findDirsToRemove :: [Directory] -> [Directory] -> [FilePath]
findDirsToRemove old newD = findDirsToAdd newD old

-- | [findDirsToAdd old new]
findDirsToAdd :: [Directory] -> [Directory] -> [FilePath]
findDirsToAdd _ [] = []
findDirsToAdd old (x : xs)
  | isInOld x old = findDirsToAdd old xs
  | otherwise     = (getPathFromDirectory x) : (findDirsToAdd old xs)

isInOld :: Directory -> [Directory] -> Bool
isInOld _ [] = False
isInOld nf (x : xs)
  | getPathFromDirectory nf == getPathFromDirectory x = True
  | otherwise                                         = isInOld nf xs

addNewDirs :: [FilePath] -> IO ()
addNewDirs []       = return ()
addNewDirs (x : xs) = do
  createDirectory x
  addNewDirs xs

