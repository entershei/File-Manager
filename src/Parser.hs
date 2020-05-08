module Parser
  (
    Command (..),
    parserCommand,
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Command
  = Cd String
  | Dir
  | CreateFolder String
  | Cat String
  | CreateFile String
  | Remove String
  | WriteToFile String String
  | FindFile String
  | FindDir String
  | Information String
  | Quit

cd :: Parser Command
cd = Cd <$> strOption
  (  long "cd"
  <> metavar "DIR-NAME"
  <> help "Change directory to the given" )

dir :: Parser Command
dir = flag' Dir
  (  long "dir"
  <> help "Show content of the current directory" )

createFolder :: Parser Command
createFolder = CreateFolder <$> strOption
  (  long "create-folder"
  <> metavar "FOLDER-NAME"
  <> help "Create a new folder in the current directory" )

cat :: Parser Command
cat = Cat <$> strOption
  (  long "cat"
  <> metavar "FILE-NAME"
  <> help "Show content of the file" )

createFile :: Parser Command
createFile = CreateFile <$> strOption
  (  long "create-file"
  <> metavar "FILE-NAME"
  <> help "Create empty file in the current directory" )

remove :: Parser Command
remove = Remove <$> strOption
  (  long "remove"
  <> metavar "FILE-NAME | DIRECTORY-NAME"
  <> help "Remove the file or the directory from the current directory" )

writeToFile :: Parser Command
writeToFile = WriteToFile <$> strOption
  (  long "write-to-file"
  <> metavar "FILE-NAME"
  <> help "File name for writing" )
  <*> strOption
  (  long "word"
  <> short 'w'
  <> metavar "WORD"
  <> help "Word that will be put into the file"
  )

findFile :: Parser Command
findFile = FindFile <$> strOption
  (  long "find-file"
  <> metavar "FILE-NAME"
  <> help "Show path of the file if it was found" )

findDir :: Parser Command
findDir = FindDir <$> strOption
  (  long "find-dir"
  <> metavar "DIRECTORY-NAME"
  <> help "Show path of the directory if it was found" )

information :: Parser Command
information = Information <$> strOption
  (  long "info"
  <> metavar "FILE-NAME | FOLDER-NAME"
  <> help "Show information of the file or folder" )

quit :: Parser Command
quit = flag' Quit
  (  long "quit"
  <> short 'q'
  <> help "Quit the program" )

parserCommand :: Parser Command
parserCommand = cd <|> dir <|> createFolder <|> cat <|> createFile <|> remove
  <|> writeToFile <|> findFile <|> findDir <|> information <|> quit

