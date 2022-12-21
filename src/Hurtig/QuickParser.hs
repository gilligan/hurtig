module Hurtig.QuickParser where

import qualified Data.List as L
import qualified Data.Text as T
import Data.Void
import Prettyprinter
import Prettyprinter.Render.String
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

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

singleQuotedString :: Parser String
singleQuotedString = char '\'' *> manyTill L.charLiteral (char '\'')

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

timeStamp :: Parser UTCTime
timeStamp = do
  year <- read <$> count 4 digitChar <* char '-'
  month <- read <$> count 2 digitChar <* char '-'
  day <- read <$> count 2 digitChar <* char ' '
  hour <- read <$> count 2 digitChar <* char ':'
  minute <- read <$> count 2 digitChar <* char ':'
  second <- read <$> count 2 digitChar <* char '.'
  ms <- read <$> count 3 digitChar
  return $ UTCTime year month day hour minute second ms

suiteParser :: (String -> UTCTime -> a) -> T.Text -> Parser a
suiteParser f str = do
  _ <- string "Test Suite" <* space1
  name <- singleQuotedString <* space1
  _ <- string str <* space1
  f name <$> timeStamp

commaSep :: Parser [String]
commaSep = sepBy (some $ alphaNumChar <|> char '.' <|> char ' ') (char ',' <* space1)

caseStartParser :: Parser QuickLog
caseStartParser = do
  _ <- string "Test Case" <* space1
  _ <- char '\''
  str <- commaSep
  _ <- char '\'' <* space1
  _ <- string "started at" <* space1
  QuickLogCaseStart str <$> timeStamp

casePassParser :: Parser QuickLog
casePassParser = do
  _ <- string "Test Case" <* space1
  _ <- char '\''
  str <- commaSep
  _ <- char '\'' <* space1
  _ <- string "passed" <* space1
  return $ QuickLogCasePass str

quickLogOutput :: Parser QuickLog
quickLogOutput = try suiteStart <|> try suitePass <|> try caseStartParser <|> try casePassParser <|> miscInfo
  where
    suiteStart = suiteParser QuickLogSuiteStart "started at"
    suitePass = suiteParser QuickLogSuitePass "passed at"

parseQuickOutput :: String -> Either (ParseErrorBundle T.Text Void) [QuickLog]
parseQuickOutput str =
  let input = T.lines . T.pack $ str
   in traverse (runParser quickLogOutput "") input

miscInfo :: Parser QuickLog
miscInfo = try buildComplete <|> buildInfo <|> linkInfo <|> execInfo
  where
    buildInfo = do
      _ <- string "Building for"
      text <- some charLiteral
      return $ QuickLogInfo $ "Building for" ++ text

    linkInfo = do
      _ <- char '['
      x <- decimal
      _ <- char '/'
      y <- decimal
      _ <- char ']' <* space1
      _ <- string "Linking" <* space1
      name <- some (alphaNumChar <|> char '.')
      return $ QuickLogInfo $ "[" ++ show x ++ "/" ++ show y ++ "] Linking " ++ name

    buildComplete = do
      _ <- string "Build complete!"
      return $ QuickLogInfo "Build complete"

    execInfo = do
      _ <- some space1
      _ <- string "Executed" <* space1
      numTests <- decimal <* space1
      _ <- string "tests, with" <* space1
      numFail <- decimal <* space1
      _ <- string "failures"
      return $ QuickLogInfo $ "Executed " ++ show numTests ++ " tests with " ++ show numFail ++ " failures"
