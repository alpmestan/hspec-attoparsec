{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Char (isDigit)
-- we import Text, this will be our input type
import Data.Text (Text)
-- we import hspec, to run the test suite
import Test.Hspec
-- we import 'hspec-attoparsec'
import Test.Hspec.Attoparsec
-- we import the module where our parser is defined
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "weird parser - success cases" $ do

    it "successfully parses |a| into 'a'" $
      ("|a|" :: Text) ~> weirdParser
        `shouldParse` 'a'

    it "successfully parses |3| into '3'" $
      ("|3|" :: Text) ~> weirdParser
        `shouldParse` '3'

    it "successfully parses ||| into '|'" $
      ("|||" :: Text) ~> weirdParser
        `shouldParse` '|'

    it "successfully parses a digit character from |3|" $
      ("|3|" :: Text) ~> weirdParser
        `parseSatisfies` isDigit

    it "successfully parses |\160|" $ 
      weirdParser `shouldSucceedOn` ("|\160|" :: Text)

  describe "weird parser - failing cases" $ do

    it "fails to parse |x-" $
      weirdParser `shouldFailOn` ("|x-" :: Text)

    it "fails to parse ||/" $
      weirdParser `shouldFailOn` ("||/" :: Text)

  describe "weird parser - leftovers" $ 
    it "leaves \"fooo\" unconsumed in |a|fooo" $
      ("|a|fooo" :: Text) ~?> weirdParser
        `leavesUnconsumed` "fooo"
