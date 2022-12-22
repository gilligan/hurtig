module Hurtig.QuickLog where

import qualified Data.List as L
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String

data UTCTime = UTCTime
  { year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int,
    ms :: Int
  }
  deriving (Show, Eq)

data QuickLog
  = -- | '''Test Suite 'All tests' started at 2022-12-17 23:08:03.162'''
    QuickLogSuiteStart
      { suiteName :: String,
        suiteStart :: UTCTime
      }
  | -- | ''' Test Suite 'All tests' passed at 2022-12-17 23:08:03.164'''
    QuickLogSuitePass
      { suiteName :: String,
        suiteFinished :: UTCTime
      }
  | -- | ''' Test Suite 'All tests' failed at 2022-12-17 23:08:03.164'''
    QuickLogSuiteFail
      { suiteName :: String,
        suiteFinished :: UTCTime
      }
  | -- | '''Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' started at 2022-12-17 23:08:03.163'''
    QuickLogCaseStart
      { testCasePath :: [String],
        testCaseStart :: UTCTime
      }
  | -- | '''Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)'''
    QuickLogCasePass
      {testCasePath :: [String]}
  | -- | '''Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)'''
    QuickLogCaseFail
      {testCasePath :: [String]}
  | -- | /Tests/QuickTests/FooTest.swift:24: error: QuickSpec.SwiftFoo, Util, composition, composes two functions : failed - expected to equal <7>, got <6>
    QuickLogExpectationFailure
      { testCasePath :: [String],
        errorInfo :: String
      }
  | -- | Any other output
    QuickLogInfo {info :: String}
  deriving (Eq, Show)

prettyUTCTime :: UTCTime -> Doc ann
prettyUTCTime (UTCTime y m d h min s ms) =
  mconcat
    [ pretty y,
      "-",
      pretty m,
      "-",
      pretty d,
      " ",
      pretty h,
      ":",
      pretty min,
      ":",
      pretty s,
      ".",
      pretty ms
    ]

printQuickLog :: QuickLog -> String
printQuickLog = renderString . layoutPretty defaultLayoutOptions . prettyQuickLog

prettyQuickLog :: QuickLog -> Doc ann
prettyQuickLog ql = case ql of
  QuickLogSuiteStart name time ->
    "Test Suite '" <> pretty name <> "' started at " <> prettyUTCTime time
  QuickLogSuitePass name time ->
    "Test Suite '" <> pretty name <> "' passed at " <> prettyUTCTime time
  QuickLogSuiteFail name time ->
    "Test Suite '" <> pretty name <> "' failed at " <> prettyUTCTime time
  QuickLogCaseStart names time ->
    let p = L.intersperse ", " $ init names
        path = hcat (map (pretty . T.pack) p)
        testDesc = last names
     in "Test Case '" <> path <> ", " <> pretty testDesc <> "' started at " <> prettyUTCTime time
  QuickLogCasePass names ->
    "Test Case '" <> hcat (map (pretty . T.pack) names) <> "' passed"
  QuickLogCaseFail names ->
    "Test Case '" <> hcat (map (pretty . T.pack) names) <> "' failed"
  QuickLogExpectationFailure p info ->
    "error: " <> hcat (map (pretty . T.pack) p) <> " failed - " <> pretty info
  QuickLogInfo s ->
    pretty s
