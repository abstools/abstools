module Utils where

import System.FilePath ((</>), replaceExtension)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import ParABS (myLexer, pProgram)
import ErrM
import Data.List (isSuffixOf)
import Control.Monad (when, liftM)

-- parse wrapper for directories (TODO: make it recursive)
parseABSFiles fileOrDir = do
  isdir <- doesDirectoryExist fileOrDir
  if isdir
    then do
      -- TODO: goes only 1 level deep in the directory, maybe FIX later
      contents <- getDirectoryContents fileOrDir
      let absFiles = filter (isSuffixOf ".abs") contents
      mapM (\ relativeFile -> parseABSFile (fileOrDir </> relativeFile)) absFiles
    else liftM return $ parseABSFile fileOrDir

-- parse wrapper for an ABS file, 
-- write the resulted AST in a file with extension .ast
-- and also returns the AST
parseABSFile absFilePath = do
  isfile <- doesFileExist absFilePath
  when (not isfile) $ error "ABS file does not exist"
  absSource <- readFile absFilePath
  let parseABS = pProgram $ myLexer absSource
  case parseABS of
    Ok res -> do
      writeFile (replaceExtension absFilePath ".ast") (show  res)
      return (absFilePath, res)
    Bad _errorString -> error "Error in parsing" -- TODO: move to exceptions
