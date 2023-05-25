{-# LANGUAGE Strict #-}

module Main ( main ) where

import Solitude hiding (State)

import Data.Aeson ( decodeFileStrict, encodeFile )
import Breadcrumbs
import System.Directory
import System.IO ( hPutStrLn )
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Options
import Test.Tasty.Ingredients
import Test.Tasty.Runners
import qualified Data.Map as M
import Urist.Parser
import Test.Tasty.HUnit
import qualified Xeno.DOM as X
import Data.Set as S
import Effectful.State.Dynamic

-- this is a rip of tasty's main, but hooking my own global `TraceID` through it for
-- better Zipkin traces.
main :: IO ()
main = runEff
  . runBreadcrumbs Nothing $
    do
      (testTree, opts) <- liftIO $ do
        let testTree = mkTree
        installSignalHandlers
        opts <- parseOptions defaultIngredients testTree
        pure (testTree, opts)
      case tryIngredients defaultIngredients opts testTree of
        Nothing -> liftIO $ do
          hPutStrLn stderr
            "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
          exitFailure
        Just act -> do
          runNo <- liftIO getAndIncrementRunNumber
          ok <- withSpan' "Test Suite" ("Run #" <> show runNo) $ do
            (TraceID s) <- getTraceId
            liftIO $ do
              writeFileBS "traceid.temp" s
              o <- liftIO act
              removeFile "traceid.temp"
              pure o
          flush
          liftIO $ if ok then exitSuccess else exitFailure

getAndIncrementRunNumber :: IO Int
getAndIncrementRunNumber = do
  ex <- doesFileExist "run_no"
  (fc :: Maybe Int) <- (if ex then decodeFileStrict "run_no" else pure Nothing)
  let fc' = fromMaybe 1 fc
  encodeFile "run_no" (fc' + 1)
  pure fc'

mkTree :: TestTree
mkTree = testGroup "can run the parser" $ [testCase "stuff" $ do
  errs <- runEff $ runBreadcrumbs Nothing $ parse "region1-00250-01-01-legends.xml" "region1-00250-01-01-legends_plus.xml"
  print errs
  error ""
  ]