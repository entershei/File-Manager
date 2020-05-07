{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystem
  (
    FileSystem (..),
    FSError (..),
    FileData (..),
    FSTree (..),
    FilesInDir (..),
    File (..),
    SubDirs (..),
    ParentDir (..),
    Type (..),
    ModificationTime (..),
    Size (..),
    FileInfo (..),
    CountFiles (..),
    DirInfo (..),
  ) where

import Control.Monad.Except (Except)
import Control.Monad.State (StateT)
import System.IO (FilePath)
import Control.Monad (Monad)
import System.Directory (Permissions (..))
import Data.Time.Clock (UTCTime (..))

data FSError = ReadFile String | WriteFile String

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

newtype SubDirs = SubDirs [FSTree]

newtype ParentDir = ParentDir FSTree

data FSTree
  = Root DirInfo FilesInDir SubDirs
  | Node DirInfo FilesInDir SubDirs ParentDir
  | Leaf DirInfo FilesInDir ParentDir

newtype FileSystem a = FileSystem
  { runFileSystem :: StateT FSTree (Except FSError) a}
  deriving (Functor, Applicative, Monad)

