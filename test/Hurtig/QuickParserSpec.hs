module Hurtig.QuickParserSpec (spec) where

import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hurtig.QuickLog
import Hurtig.QuickParser
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Hurtig" $ do
    describe "QuickParser" $ do
      describe "timeStamp" $ do
        it "should parse time stamp of the format YYYY-MM-DD HH:MM:SS.MS" $ do
          case runParser timeStamp "" "2022-12-17 23:08:03.162" of
            Right res -> res `shouldBe` UTCTime 2022 12 17 23 8 3 162
            Left x -> expectationFailure (show x)
      describe "quickLogOutput" $ do
        it "parses the start of a test suite" $ do
          case runParser quickLogOutput "" "Test Suite 'All Tests' started at 2022-12-17 23:08:03.162" of
            Right res -> res `shouldBe` QuickLogSuiteStart "All Tests" (UTCTime 2022 12 17 23 8 3 162)
            Left x -> expectationFailure $ show x
        it "parses the successful passing of a test suite" $ do
          case runParser quickLogOutput "" "Test Suite 'All Tests' passed at 2022-12-17 23:08:03.162" of
            Right res -> res `shouldBe` QuickLogSuitePass "All Tests" (UTCTime 2022 12 17 23 8 3 162)
            Left x -> expectationFailure $ show x
        it "parses the start of a test case" $ do
          case runParser quickLogOutput "" "Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' started at 2022-12-17 23:08:03.163" of
            Right res -> res `shouldBe` QuickLogCaseStart ["QuickSpec.SwiftFoo", "Util", "getInput", "reads data from file"] (UTCTime 2022 12 17 23 8 3 163)
            Left x -> expectationFailure $ show x
        it "parses the passing of a test case" $ do
          case runParser quickLogOutput "" "Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)" of
            Right res -> res `shouldBe` QuickLogCasePass ["QuickSpec.SwiftFoo", "Util", "getInput"] "reads data from file"
            Left x -> expectationFailure $ show x
        it "parses Building Info output as QuickLogInfo" $ do
          case runParser quickLogOutput "" "Building for debugging..." of
            Right res -> res `shouldBe` QuickLogInfo "Building for debugging..."
            Left x -> expectationFailure $ show x
        it "parses Linking Info output as QuickLogInfo" $ do
          case runParser quickLogOutput "" "[13/13] Linking SwiftFooPackageTest.xctest" of
            Right res -> res `shouldBe` QuickLogInfo "[13/13] Linking SwiftFooPackageTest.xctest"
            Left x -> expectationFailure $ show x
        it "parses Build completion Info output as QuickLogInfo" $ do
          case runParser quickLogOutput "" "Build complete! (1.78s)" of
            Right res -> res `shouldBe` QuickLogInfo "Build complete"
            Left x -> expectationFailure $ show x
        it "parses Test Execution Info output as QuickLogInfo" $ do
          case runParser quickLogOutput "" "         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds" of
            Right res -> res `shouldBe` QuickLogInfo "Executed 4 tests with 0 failures"
            Left x -> expectationFailure $ show x
        it "parses an expectation failure" $ do
          case runParser quickLogOutput "" "/Tests/QuickTests/FooTest.swift:24: error: QuickSpec.SwiftFoo, Util, composition, composes two functions : failed - expected to equal <7>, got <6>" of
            Right res -> res `shouldBe` QuickLogExpectationFailure ["QuickSpec.SwiftFoo", "Util", "composition", "composes two functions"] "expected to equal <7>, got <6>"
            Left x -> expectationFailure $ show x
        it "parses the full output of a successful run" $ do
          successOutput <- T.lines <$> TIO.readFile "./smoke-tests/quick-simple-success.in"
          let res = traverse (runParser quickLogOutput "") successOutput
          isRight res `shouldBe` True
        it "parses the full output of a failed run" $ do
          failedOutput <- T.lines <$> TIO.readFile "./smoke-tests/quick-failed-test.in"
          let res = traverse (runParser quickLogOutput "") (filter (/= "") failedOutput)
          case res of
            Right _ -> pure ()
            Left err -> expectationFailure $ show err
