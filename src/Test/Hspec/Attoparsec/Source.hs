{-# LANGUAGE MultiParamTypeClasses,
			 TypeSynonymInstances,
			 FlexibleInstances,
			 FunctionalDependencies #-}
-- |
-- Module       : Test.Hspec.Attoparsec.Source
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- 
-- A 'Source' class that ties parser types and input types to
-- give you a uniform interface for testing your parsers,
-- without caring about the input type.
module Test.Hspec.Attoparsec.Source where 

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as B

import qualified Data.Attoparsec.ByteString.Lazy as ALB
import qualified Data.ByteString.Lazy as LB

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T

import qualified Data.Attoparsec.Text.Lazy as ALT
import qualified Data.Text.Lazy as LT

import qualified Data.Attoparsec.Types as Atto

import Data.String (IsString)

-- | A class where each instance will just teach
--   how to get an Either or the specific result 
--   type associated to the parser for the given
--   input type.
class (Eq string, Show string, IsString string)
   => Source parser string string' result | string -> parser, string -> result, string -> string' where
	-- | Feed some input to a parser and extract the result
	--   as either a failure 'String' or an actually parsed value.
	--   Can be read as /fed to/.
	-- 
	-- > -- "<a ...>" fed to an HTML parser 
	-- > "<a href=\"/foo\">Go to foo</a>" ~> htmlParser :: Either String a
	(~>) :: string -> parser string' a -> Either String a

	-- | Feed some input to a parser and extract it as the
	--   appropriate result type from that module.
	-- 
	--   This is not currently useful in the library per se,
	--   but is used in test-suites directly where we generally only deal
	--   with one concrete set of parser, input and result types.
	--   This lets us inspect the result in any way we want, e.g
	--   in conjunction with @shouldSatisfy@ or a custom hspec combinator.
	(~?>) :: string -> parser string' a -> result a

instance Source Atto.Parser B.ByteString B.ByteString AB.Result where
	t ~> p = AB.eitherResult $ t ~?> p

	t ~?> p = AB.parse p t

instance Source Atto.Parser LB.ByteString B.ByteString ALB.Result where
	t ~> p = ALB.eitherResult $ t ~?> p

	t ~?> p = ALB.parse p t

instance Source Atto.Parser T.Text T.Text AT.Result where
	t ~> p = AT.eitherResult $ t ~?> p

	t ~?> p = AT.parse p t

instance Source Atto.Parser LT.Text T.Text ALT.Result where
	t ~> p = ALT.eitherResult $ t ~?> p

	t ~?> p = ALT.parse p t

class Leftover r s | r -> s where
	leftover :: r a -> Maybe s

instance Leftover AB.Result B.ByteString where
	leftover (AB.Done t _) = Just t
	leftover _             = Nothing

instance Leftover ALB.Result LB.ByteString where
	leftover (ALB.Done t _) = Just t
	leftover _              = Nothing

instance Leftover AT.Result T.Text where
	leftover (AT.Done t _) = Just t
	leftover _            = Nothing

instance Leftover ALT.Result LT.Text where
	leftover (ALT.Done t _) = Just t
	leftover _              = Nothing
