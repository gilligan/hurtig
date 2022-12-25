{-# LANGUAGE DeriveTraversable #-}

module Hurtig.TestTree where

import qualified Data.List as L
import Hurtig.QuickLog

data TestTree a
  = TestTree String [TestTree a]
  | TestSpec a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TestInfo = TestInfo
  { passed :: Bool,
    desc :: String
  }
  deriving (Eq, Show)

type QuickTestTree = TestTree TestInfo

insert :: QuickLog -> QuickTestTree -> QuickTestTree
insert (QuickLogCasePass _ _) (TestSpec x) = TestSpec x
insert (QuickLogCasePass path exp) tree@(TestTree name [])
  | length path == 1 && head path == name = TestTree name [TestSpec $ TestInfo True exp]
  | length path > 1 && head path == name = TestTree name [insert (QuickLogCasePass (tail path) exp) (TestTree (head $ tail path) [])]
  | otherwise = tree
insert (QuickLogCasePass path exp) tree@(TestTree name ts)
  | length path == 1 && head path == name = TestTree name (TestSpec (TestInfo True exp) : ts)
  | length path > 1 && head path == name = TestTree name (insert (QuickLogCasePass (tail path) exp) <$> ts)
  | otherwise = tree
insert _ tree = tree

fromQuickLog :: [QuickLog] -> QuickTestTree
fromQuickLog qs = foldr insert (TestTree "QuickSpec" []) (sanitizeTestCase <$> filter isTestCase qs)
  where
    isTestCase :: QuickLog -> Bool
    isTestCase (QuickLogCasePass _ _) = True
    isTestCase _ = False
    sanitizeTestCase :: QuickLog -> QuickLog
    sanitizeTestCase q@(QuickLogCasePass [] _) = q
    sanitizeTestCase q@(QuickLogCasePass (p : ps) _)
      | "QuickSpec." `L.isPrefixOf` p = q {testCasePath = "QuickSpec" : drop 10 p : ps}
      | otherwise = q
    sanitizeTestCase x = x
