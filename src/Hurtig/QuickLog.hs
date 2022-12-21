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
    QuickLogSuiteStart String UTCTime
  | -- | ''' Test Suite 'All tests' passed at 2022-12-17 23:08:03.164'''
    QuickLogSuitePass String UTCTime
  | -- | '''Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' started at 2022-12-17 23:08:03.163'''
    QuickLogCaseStart [String] UTCTime
  | -- | '''Test Case 'QuickSpec.SwiftFoo, Util, getInput, reads data from file' passed (0.001 seconds)'''
    QuickLogCasePass [String]
  | -- | Any other output
    QuickLogInfo String
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
  QuickLogCaseStart names time ->
    let p = L.intersperse ", " $ init names
        path = hcat (map (pretty . T.pack) p)
        testDesc = last names
     in "Test Case '" <> path <> ", " <> pretty testDesc <> "' started at " <> prettyUTCTime time
  QuickLogCasePass names ->
    "Test Case '" <> hcat (map (pretty . T.pack) names) <> "' passed"
  QuickLogInfo s ->
    pretty s