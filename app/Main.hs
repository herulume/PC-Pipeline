{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cp
import Pipeline
import Turtle

parser :: Parser (Text, Text)
parser =
  (,) <$> optText "log" 'l' "Log file"
    <*> optText "dir" 'd' "Directory to process"

main :: IO ()
main = options "CP" parser >>= (uncurry processFiles) . (fromText >< fromText)
