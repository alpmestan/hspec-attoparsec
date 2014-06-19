module Parser where

import Data.Attoparsec.Text

weirdParser :: Parser Char
weirdParser = do -- attoparsec's Parser type has a useful monad instance
  char '|'       -- matches just '|', fails on any other char
  c <- anyChar   -- matches any character (but just one) and returns it
  char '|'       -- matches just '|', like on the first line
  return c       -- return the inner character we parsed