{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Test.Hspec.Attoparsec
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- Utility functions for testing @attoparsec@ parsers
-- 
-- You can use `(~>)` on different input types
-- 
-- * Strict 'Data.ByteString.ByteString'
-- * Lazy 'Data.ByteString.Lazy.ByteString'
-- * Strict 'Data.Text.Text'
-- * Lazy 'Data.Text.Lazy.Text'
--
module Test.Hspec.Attoparsec
  ( -- * Equality-based combinator
    shouldParse

  , -- * Predicate-based combinator
    parseSatisfies

  , -- * Inspecting the result
    shouldSucceedOn
  , shouldFailOn

  , -- * Inspecting unconsumed input
    leavesUnconsumed

  , -- * The 'Source' class, connecting parsers and inputs
    Source(..)

  , -- * The 'Leftover' class, letting us inspect unconsumed input
    Leftover(..)
  ) where

import Control.Monad (when)
import Data.Either (isLeft, isRight)
import Test.Hspec.Attoparsec.Source
import Test.Hspec.Expectations

-- | Create an expectation by saying what the result should be.
--   Intended to be used with @(~>)@ as follows:
--
-- >   "<!-- foo -->" ~> htmlCommentParser
-- >     `shouldParse` TagComment " foo "
shouldParse :: (Eq a, Show a) => Either String a -> a -> Expectation
res `shouldParse` expectedVal =
  either (expectationFailure . errmsg)
         checkEquality
         res

  where errmsg err =   "  expected: " ++ show expectedVal
                  ++ "\n  but parsing failed with error: " ++ err

        checkEquality parsedVal =
          when (parsedVal /= expectedVal) $
            expectationFailure $   "  expected: " ++ show expectedVal
                              ++ "\n  but got: " ++ show parsedVal

-- | Create an expectation by saying that the parser should successfully
--   parse a value and that this value should satisfy some predicate.
--   
--   This can fail if the parsing doesn't succeed or if it succeeds but
--   the value doesn't match the predicate.
--
-- > ">>>" ~> many (char '>')
-- >   `parseSatisfies` ((==3) . length)
parseSatisfies :: Show a => Either String a -> (a -> Bool) -> Expectation
parseSatisfies res predicate =
  either (expectationFailure . errmsg)
         checkPred
         res

  where errmsg err =   "  expected a parsed value to check against the predicate"
                  ++ "\n  but parsing failed with error: " ++ err

        checkPred value =
          when (not $ predicate value) $
            expectationFailure $   
                 "  the following value did not match the predicate: \n"
              ++ "  " ++ show value

-- | Check that a parser fails on some given input
--
-- > char 'x' `shouldFailOn` "a"
shouldFailOn :: (Source p s s' r, Show a)
             => p s' a
             -> s
             -> Expectation
parser `shouldFailOn` string =
  (string ~> parser) `shouldSatisfy` isLeft

-- | Check that a parser succeeds on some given input
--
-- > char 'x' `shouldSucceedOn` "x"
shouldSucceedOn :: (Source p s s' r, Show a)
                => p s' a
                -> s
                -> Expectation
parser `shouldSucceedOn` string =
  (string ~> parser) `shouldSatisfy` isRight

-- | Checking that the given parser succeeds
--   and yields the given part of the input unconsumed
leavesUnconsumed :: (Source p s s' r, Leftover r s)
                 => r a
                 -> s
                 -> Expectation
leavesUnconsumed res str
  | unconsumed == Nothing && str /= "" =
      expectationFailure $ 
           "  expected the parser to leave the following unconsumed: " ++ show str
      ++ "\n  but got no leftover"
  | otherwise = 
      case unconsumed of
        Just str' -> 
          when (str /= str') $
            expectationFailure $ 
                   "  expected the parser to leave the following unconsumed: " ++ show str
              ++ "\n  but got: " ++ show str'

  where unconsumed = leftover res





