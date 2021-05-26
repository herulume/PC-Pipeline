{-# LANGUAGE OverloadedStrings #-}

module Pipeline (processFiles) where

import Control.Concurrent.Async (mapConcurrently)
import Cp
import Pipeline.Files
import Pipeline.Props
import Pipeline.Utils
import Turtle
import Prelude hiding (FilePath)

runTests :: Text -> FilePath -> Text -> IO ()
runTests group logF file = do
  let log = (show group) <> "\n"
      appendT = appendFile (encodeString logF)
  compile <- compilePred file
  if compile
    then do
      failedProps <- failProps file props
      changedTypes <- failTypes file props
      appendT $ log <> "Failed props: " <> show failedProps <> "\n" <> "Changed types: " <> show changedTypes <> "\n\n"
    else appendT $ log <> "Didn't compile\n\n"

unzipAndRun :: (Text -> Text -> IO ()) -> FilePath -> FilePath -> IO ()
unzipAndRun unzipFun logF file = runManaged $ do
  outDir <- fmap fpToText $ mktempdir "." "tmp"
  let fileToUnzip = fpToText file
      lhs = outDir <> "/" <> lhsFile
  liftIO $ unzipFun outDir fileToUnzip
  liftIO $ runTests fileToUnzip logF lhs

processFiles :: FilePath -> FilePath -> IO ()
processFiles logF dir = do
  let runt = unzipAndRun unzipFile logF
      runtStack = unzipAndRun unzipStackFile logF
  files <- fmap (conc . ((map i1) >< (map i2))) . lsZips $ dir
  _ <- mapConcurrently (either runt runtStack) files
  print ("Done" :: String)
