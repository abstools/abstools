{-# LANGUAGE DeriveDataTypeable #-}

module Conf where

import System.IO.Unsafe (unsafePerformIO)
import System.Console.CmdArgs

conf = unsafePerformIO getConf

getConf = cmdArgs confOpt

data Conf = Conf {
      files :: [FilePath],
      main_is :: FilePath
    } deriving (Show, Eq, Data, Typeable)

confOpt = Conf {
          files = def &= args &= typ "FILES/DIRS",
          main_is = "Main.hs" &= name "main-is"  &= typ "FILE"
          }
          &= program "abs2haskell" &= help "ABS to Haskell transpiler" &= summary "abs2haskell v0.0.1, Nikolaos Bezirgiannis, Envisage Project"

