{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import urist-mcbrowser
import Relude

data Bar = F deriving Show
main :: IO ()
main = putStrLn ("hi" :: String)