{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.AttoparsecSpec where

import Data.Attoparsec.Text
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "shouldParse" $
    it "works on: \"x\" ~> char 'x'" $
      ("x" :: Text) ~> char 'x'
        `shouldParse` 'x'

  describe "parseSatisfies" $
    it "works on: \"x\" and (=='x')" $
      ("x" :: Text) ~> char 'x'
        `parseSatisfies` (=='x')

  describe "shouldFailOn" $
    it "char 'x' fails on \"ha\"" $
      char 'x' `shouldFailOn` ("ha" :: Text)

  describe "shouldSucceedOn" $
    it "char 'x' succeeds on \"x\"" $
      char 'x' `shouldSucceedOn` ("x" :: Text)

  describe "leavesUnconsumed" $
    it "works on \"xa\" ~?> char 'x'" $
      ("xa" :: Text) ~?> char 'x'
        `leavesUnconsumed` "a"

