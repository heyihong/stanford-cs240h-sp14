module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False

    describe "literal pattern" $ do
      it "matches string" $
        matchGlob "string" "string" `shouldBe` True
      it "shouldn't match string1" $
        matchGlob "string" "stringa" `shouldBe` False
      it "shouldn't match string2" $
        matchGlob "string" "astring" `shouldBe` False
      it "escaped literals 1" $
        matchGlob "\\[a]" "[a]" `shouldBe` True
      it "escaped literals 2" $
        matchGlob "\\*\\*\\?" "**?" `shouldBe` True
      it "escaped literals 3" $
        matchGlob "\\\\a\\\\" "\\a\\" `shouldBe` True
      it "escaped literals 4" $
        matchGlob "ab\\*ba" "ab*ba" `shouldBe` True
      it "escaped literals 5" $
        matchGlob "ab\\[ba" "ab[ba" `shouldBe` True
      it "escaped literals 6" $
        matchGlob "ab[a\\]]ba" "ab]ba" `shouldBe` True
      it "escaped literals 7" $
        matchGlob "ab[a\\]]ba" "ababa" `shouldBe` True

    describe "bracket pattern" $ do
        it "simple bracket" $
            matchGlob "a[bc]d" "acd" `shouldBe` True
        it "contiguous bracket" $
            matchGlob "a[b-z]d" "add" `shouldBe` True


    describe "set match" $ do
        describe "[ab[c]" $ do
            it "matches a" $
                matchGlob "[ab[c]" "a" `shouldBe` True
            it "matches b" $
                matchGlob "[ab[c]" "b" `shouldBe` True
            it "matches [" $
                matchGlob "[ab[c]" "[" `shouldBe` True
            it "matches c" $
                matchGlob "[ab[c]" "c" `shouldBe` True
        describe "[abcd]" $ do
            it "matches a" $
                matchGlob "[abcd]" "a" `shouldBe` True
            it "matches b" $
                matchGlob "[abcd]" "b" `shouldBe` True
            it "matches c" $
                matchGlob "[abcd]" "c" `shouldBe` True
            it "matches d" $
                matchGlob "[abcd]" "d" `shouldBe` True
        describe "[a-z]" $ do
            it "matches a" $
                matchGlob "[a-z]" "a" `shouldBe` True
            it "matches b" $
                matchGlob "[a-z]" "b" `shouldBe` True
            it "matches y" $
                matchGlob "[a-z]" "y" `shouldBe` True
            it "matches z" $
                matchGlob "[a-z]" "z" `shouldBe` True
        describe "[-abc]" $ do
            it "matches a" $
                matchGlob "[-abc]" "-" `shouldBe` True
            it "matches b" $
                matchGlob "[-abc]" "a" `shouldBe` True
            it "matches c" $
                matchGlob "[-abc]" "b" `shouldBe` True
            it "matches z" $
                matchGlob "[-abc]" "c" `shouldBe` True
        describe "[abc-]" $ do
            it "matches a" $
                matchGlob "[abc-]" "a" `shouldBe` True
            it "matches b" $
                matchGlob "[abc-]" "b" `shouldBe` True
            it "matches c" $
                matchGlob "[abc-]" "c" `shouldBe` True
            it "matches -" $
                matchGlob "[abc-]" "-" `shouldBe` True
        describe "[--]" $ do
            it "matches -" $
                matchGlob "[--]" "-" `shouldBe` True
        describe "[---]" $ do
            it "matches -" $
                matchGlob "[---]" "-" `shouldBe` True
        describe "[----]" $ do
            it "matches -" $
                matchGlob "[----]" "-" `shouldBe` True
        describe "[a-d-z]" $ do
            it "matches a" $
                matchGlob "[a-d-z]" "a" `shouldBe` True
            it "matches b" $
                matchGlob "[a-d-z]" "b" `shouldBe` True
            it "matches c" $
                matchGlob "[a-d-z]" "c" `shouldBe` True
            it "matches d" $
                matchGlob "[a-d-z]" "d" `shouldBe` True
            it "matches -" $
                matchGlob "[a-d-z]" "-" `shouldBe` True
            it "matches z" $
                matchGlob "[a-d-z]" "z" `shouldBe` True
        describe "[z-a]" $ do
            it "shouldn't matches anything" $
                matchGlob "[z-a]" "a" `shouldBe` False    
            it "shouldn't matches anything" $
                matchGlob "[z-a]" "z" `shouldBe` False  

    describe "time complexity" $ do
        it "should match" $
            matchGlob "test1****test2****test3" ("test1" ++ take 1000 (repeat 'a') ++ "test2" ++ take 1000 (repeat 'b') ++ "test3") `shouldBe` True
        it "shouldn't match" $
            matchGlob "test1****test2****test" ("test1" ++ take 1000 (repeat 'a') ++ "test2" ++ take 1000 (repeat 'b') ++ "test3") `shouldBe` False 
