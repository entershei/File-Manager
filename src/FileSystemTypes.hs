{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystemTypes
  (
    CountFiles (..),
    CurrentDir (..),
    Directories (..),
    Directory (..),
    DirInfo (..),
    File (..),
    FileData (..),
    FileInfo (..),
    FilesInDir (..),
    FileSystem,
    Info (..),
    FSError (..),
    ModificationTime (..),
    Names (..),
    RelaitiveName (..),
    Size (..),
    Type (..),
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Time.Clock (UTCTime (..))
import System.Directory (Permissions (..))
import System.IO (FilePath)

-- | Main type for file system program.
type FileSystem e a = ExceptT e (State Directories) a

data FSError =  CanNotFindFile FilePath
              | CanNotFindDir FilePath
              | CanNotFindFleOrDir FilePath
              | CanNotMakeCD FilePath
              | CanNotGoHigherThanRoot

instance Show FSError where
  show (CanNotFindFile s)     = "Can't find file: " ++ s
  show (CanNotFindDir s)      = "Can't find dir: " ++ s
  show (CanNotFindFleOrDir s) = "Can't find file or directory: " ++ s
  show (CanNotMakeCD s)       = "Can't make cd to " ++ s
  show CanNotGoHigherThanRoot = "Can't go to higher dir then the root"

data Directories = Directories [Directory] CurrentDir

data Directory = Directory DirInfo FilesInDir

data DirInfo = DirInfo FilePath Size CountFiles Permissions

newtype FilesInDir = FilesInDir [File]

newtype CurrentDir = CurrentDir FilePath

newtype FileData = FileData (String)

instance Show FileData where
  show (FileData str) = "Text in the file: " ++ str

data Type = NoExecutable | Executable

instance Show Type where
  show NoExecutable = "no-executable"
  show Executable   = "executable"

newtype ModificationTime = ModificationTime UTCTime
  deriving (Show)

-- | Size of file or directory in bits
newtype Size = Size Integer

data FileInfo = FileInfo FilePath Permissions Type ModificationTime Size

newtype CountFiles = CountFiles Int

newtype File = File (FileInfo, FileData)

data RelaitiveName = RelativeNameFile String | RelativeNameDirectory String
newtype Names = Names [RelaitiveName]

instance Show Names where
  show (Names [])       = ""
  show (Names (x : xs)) = show x ++ "\n" ++ show (Names xs)

instance Show RelaitiveName where
  show (RelativeNameFile f)      = "[file]: " ++ f
  show (RelativeNameDirectory d) = "[dir]: " ++ d

data Info = InfoForFile FileInfo | InfoForDir DirInfo

instance Show Info where
  show (InfoForFile (FileInfo p pm t time (Size sz))) = "File path: " ++ p
    ++ "\nPermissions: " ++ show pm
    ++ "\nType: " ++ show t
    ++ "\nLastModification: " ++ show time
    ++ "\nSize (in bytes): " ++ show sz
  show (InfoForDir (DirInfo p (Size sz) (CountFiles c) pm))
    = "Directory path: " ++ p
    ++ "\nSize (in bytes): " ++ show sz
    ++ "\nCount files in the directory: " ++ show c
    ++ "\nPermissions: " ++ show pm
