module Hurtig.QuickParserSpec (spec) where

import Data.Either
import qualified Data.Text as T
import Hurtig.QuickLog
import Hurtig.QuickParser
import Test.Hspec
import Text.Megaparsec

sampleSuccessOutput :: T.Text
sampleSuccessOutput =
  T.unlines
    [ "Building for debugging...",
      "[13/13] Linking SwiftFooPackageTests.xctest",
      "Build complete! (1.78s)",
      "Test Suite 'All tests' started at 2022-12-17 23:08:03.162",
      "Test Suite 'debug.xctest' started at 2022-12-17 23:08:03.163",
      "Test Suite 'QuickSpec' started at 2022-12-17 23:08:03.163",
      "Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' started at 2022-12-17 23:08:03.163",
      "Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)",
      "Test Case 'QuickSpec.SwiftFoo, Util, getInput, applies the function to the file contents if specified' started at 2022-12-17 23:08:03.164",
      "Test Case 'QuickSpec.SwiftFoo, Util, getInput, applies the function to the file contents if specified' passed (0.0 seconds)",
      "Test Case 'QuickSpec.SwiftFoo, Day1, part1, finds the total of the elve carrying the most calories' started at 2022-12-17 23:08:03.164",
      "Test Case 'QuickSpec.SwiftFoo, Day1, part1, finds the total of the elve carrying the most calories' passed (0.0 seconds)",
      "Test Case 'QuickSpec.SwiftFoo, Day1, part2, finds the sum of the top 3 elves' started at 2022-12-17 23:08:03.164",
      "Test Case 'QuickSpec.SwiftFoo, Day1, part2, finds the sum of the top 3 elves' passed (0.0 seconds)",
      "Test Suite 'QuickSpec' passed at 2022-12-17 23:08:03.164",
      "         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds",
      "Test Suite 'debug.xctest' passed at 2022-12-17 23:08:03.164",
      "         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds",
      "Test Suite 'All tests' passed at 2022-12-17 23:08:03.164",
      "         Executed 4 tests, with 0 failures (0 unexpected) in 0.001 (0.001) seconds"
    ]

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
            Right res -> res `shouldBe` QuickLogCasePass ["QuickSpec.SwiftFoo", "Util", "getInput", "reads data from file"]
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
        it "parses the full output of a successful run" $ do
          let res = traverse (runParser quickLogOutput "") (T.lines sampleSuccessOutput)
          isRight res `shouldBe` True
