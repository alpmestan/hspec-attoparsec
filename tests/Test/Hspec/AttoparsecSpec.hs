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
      ("x" :: Text) ~> parseX
        `shouldParse` 'x'

  describe "leavesUnconsumed" $
    it "works on \"xa\" ~?> char 'x'" $
      ("xa" :: Text) ~?> parseX
        `leavesUnconsumed` ("a" :: Text)

parseX :: Parser Char
parseX = char 'x'
