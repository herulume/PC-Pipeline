{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Utils where

import Turtle
import Prelude hiding (FilePath)

procError :: ExitCode -> IO ()
procError ExitSuccess = return ()
procError (ExitFailure n) = die ("Failed with exit code: " <> repr n)

fpToText :: FilePath -> Text
fpToText = either id id . toText

lhsFile :: Text
lhsFile = "cp2021t.lhs"

tFile :: Pattern Text
tFile = "cp2021t.zip"

tFileStack :: Pattern Text
tFileStack = "cp2021tStack.zip"

insideZipStack :: Text
insideZipStack = "\"cp2021tStack/app/cp2021t.lhs\""

insideZip :: Text
insideZip = "\"cp2021t/cp2021t.lhs\""
