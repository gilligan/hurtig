module Hurtig.TestTreeSpec (spec) where

import Hurtig.QuickLog
import Hurtig.TestTree
import Test.Hspec

spec :: Spec
spec = do
  describe "TestTree" $ do
    describe "insertTo" $ do
      it "adds a 'Foo/does foo' to an empty tree" $
        insertTo ["Foo"] (TestSpec (TestInfo True "does foo")) (TestTree "Foo" [])
          `shouldBe` TestTree
            "Foo"
            [ TestSpec (TestInfo True "does foo")
            ]
      it "adds a 'Foo,Bar,Baz/does foo' to an empty tree" $
        insertTo ["Foo", "Bar", "Baz"] (TestSpec (TestInfo True "does foo")) (TestTree "Foo" [])
          `shouldBe` TestTree
            "Foo"
            [ TestTree
                "Bar"
                [ TestTree
                    "Baz"
                    [TestSpec (TestInfo True "does foo")]
                ]
            ]
      it "adds a 'Foo,Bar,Baz/does foo' to tree containing Foo,Bar,Baz" $
        insertTo ["Foo", "Bar", "Baz"] (TestSpec (TestInfo True "does foo")) (TestTree "Foo" [TestTree "Bar" [TestTree "Baz" []]])
          `shouldBe` TestTree
            "Foo"
            [ TestTree
                "Bar"
                [ TestTree
                    "Baz"
                    [TestSpec (TestInfo True "does foo")]
                ]
            ]
      it "adds a 'Foo,Bar,Baz/does foo' to tree containing Foo,(Bar/does bar),Baz" $
        insertTo ["Foo", "Bar", "Baz"] (TestSpec (TestInfo True "does foo")) (TestTree "Foo" [TestTree "Bar" [TestSpec (TestInfo True "does bar"), TestTree "Baz" []]])
          `shouldBe` TestTree
            "Foo"
            [ TestTree
                "Bar"
                [ TestSpec (TestInfo True "does bar"),
                  TestTree
                    "Baz"
                    [TestSpec (TestInfo True "does foo")]
                ]
            ]
      it "adds test paths when necessary" $
        insertTo ["A", "Z"] (TestSpec (TestInfo True "bar")) (TestTree "A" [TestTree "B" [TestTree "C" [TestSpec (TestInfo True "foo")]]])
          `shouldBe` TestTree
            "A"
            [ TestTree "B" [TestTree "C" [TestSpec (TestInfo True "foo")]],
              TestTree "Z" [TestSpec (TestInfo True "bar")]
            ]

    describe "fromQuickLog" $ do
      it "is identical to insert for a single item" $
        fromQuickLog [QuickLogCasePass ["QuickSpec", "Foo"] "does foo"]
          `shouldBe` TestTree
            "QuickSpec"
            [ TestTree "Foo" [TestSpec (TestInfo True "does foo")]
            ]
      it "turns 'QuickSpec.X' into 'QuickSpec, X" $
        fromQuickLog [QuickLogCasePass ["QuickSpec.Foo"] "does foo"]
          `shouldBe` TestTree
            "QuickSpec"
            [ TestTree "Foo" [TestSpec (TestInfo True "does foo")]
            ]
      it "adds multiple items" $
        fromQuickLog [QuickLogCasePass ["QuickSpec", "Foo"] "does foo", QuickLogCasePass ["QuickSpec", "Foo"] "does bar"]
          `shouldBe` TestTree
            "QuickSpec"
            [ TestTree
                "Foo"
                [ TestSpec (TestInfo True "does foo"),
                  TestSpec (TestInfo True "does bar")
                ]
            ]
