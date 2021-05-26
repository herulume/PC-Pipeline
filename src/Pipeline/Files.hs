{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Files ( lsZips
             , unzipStackFile
             , unzipFile
             ) where

import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Cp (false)
import Pipeline.Utils (procError, insideZipStack, insideZip, tFile, tFileStack)

lsZips :: MonadIO a => FilePath -> a ([FilePath], [FilePath])
lsZips file = fold (ls file) (liftA2 (,) zipFold zipStackFold) where
  isZip name   = either false (not . null . match (suffix name)) . toText
  zipFold      = Fold.handles (Fold.filtered (isZip tFile))      Fold.list
  zipStackFold = Fold.handles (Fold.filtered (isZip tFileStack)) Fold.list

unzipStackFile :: Text -> Text -> IO ()
unzipStackFile = unzipFileGeneric insideZipStack

unzipFile :: Text -> Text -> IO ()
unzipFile = unzipFileGeneric insideZip

unzipFileGeneric :: Text -> Text -> Text -> IO ()
unzipFileGeneric iZip outDir file =  shell command empty >>= procError  where
  command = "unzip -j " <> file <> " " <> iZip <> " -d " <> outDir
