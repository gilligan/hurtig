module Hurtig.QuickParser where

import qualified Data.Text as T
import Data.Void
import Hurtig.QuickLog
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

singleQuotedString :: Parser String
singleQuotedString = char '\'' *> manyTill L.charLiteral (char '\'')

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

caseFailParser :: Parser QuickLog
caseFailParser = do
  _ <- string "Test Case" <* space1
  _ <- char '\''
  str <- commaSep
  _ <- char '\'' <* space1
  _ <- string "failed" <* space1
  return $ QuickLogCaseFail str

expectationFailParser :: Parser QuickLog
expectationFailParser = do
  _ <- pathAndLineNo
  space1
  _ <- string "error:" <* space1
  casePath <- commaSep
  _ <- string ": failed - "
  -- TODO: I am only dropping whitespace here because i failed to adjust
  --       commaSep to not include trailing whitespace. I should figure
  --       this out ;/
  QuickLogExpectationFailure (dropWhiteSpace <$> casePath) . T.unpack <$> takeRest
  where
    pathAndLineNo = char '/' *> manyTill anySingle (char ':') <* decimal <* char ':'
    dropWhiteSpace = reverse . dropWhile (== ' ') . reverse

quickLogOutput :: Parser QuickLog
quickLogOutput = try suiteStart <|> try suitePass <|> try suiteFail <|> try caseStartParser <|> try casePassParser <|> try caseFailParser <|> try expectationFailParser <|> miscInfo
  where
    suiteStart = suiteParser QuickLogSuiteStart "started at"
    suitePass = suiteParser QuickLogSuitePass "passed at"
    suiteFail = suiteParser QuickLogSuiteFail "failed at"

parseQuickOutput :: String -> Either (ParseErrorBundle T.Text Void) [QuickLog]
parseQuickOutput str =
  let input = T.lines . T.pack $ str
   in traverse (runParser quickLogOutput "") input

miscInfo :: Parser QuickLog
miscInfo = try buildComplete <|> buildInfo <|> linkInfo <|> execInfo <|> emptyString
  where
    emptyString = do
      eof
      return $ QuickLogInfo ""
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
      _ <- string "failures" <|> string "failure"
      return $ QuickLogInfo $ "Executed " ++ show numTests ++ " tests with " ++ show numFail ++ " failures"
