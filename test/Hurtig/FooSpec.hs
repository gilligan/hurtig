module Hurtig.FooSpec (spec) where

import Hurtig.Foo
import Test.Hspec

spec :: Spec
spec = do
  describe "Hurtig" $ do
    describe "nothing" $ do
      it "should just succeed" $ do
        foo `shouldBe` 1
