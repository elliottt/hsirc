-- | Parsec parsers and a general parsing interface for IRC messages
module Network.IRC.Parser (
    -- * Parsing and Formatting Functions
    decode -- :: String -> Maybe Message

    -- * Parsec Combinators for Parsing IRC messages
  , prefix         -- :: CharParser st Prefix
  , serverPrefix   -- :: CharParser st Prefix
  , nicknamePrefix -- :: CharParser st Prefix
  , command        -- :: CharParser st Command
  , parameter      -- :: CharParser st Parameter
  , message        -- :: CharParser st Message
  , crlf           -- :: CharParser st ()
  , spaces         -- :: CharParser st ()

    -- * Other Parser Combinators
  , tokenize  -- :: CharParser st a -> CharParser st a
  , takeUntil -- :: String -> CharParser st String

    -- * Deprecated Functions
  , parseMessage
  ) where

import Network.IRC.Base

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

-- | Parse a String into a Message.
decode :: String        -- ^ Message string
       -> Maybe Message -- ^ Parsed message
decode = (either (const Nothing) Just) . (parse message "")

-- | The deprecated version of decode
parseMessage :: String -> Maybe Message
parseMessage  = decode

-- | Take all tokens until one character from a given string is found
takeUntil :: String -> CharParser st String
takeUntil s = anyChar `manyTill` (lookAhead (oneOf s))

-- | Convert a parser that consumes all space after it
tokenize  :: CharParser st a -> CharParser st a
tokenize p = p >>= \x -> spaces >> return x

-- | Consume only spaces tabs or the bell character
spaces :: CharParser st ()
spaces  = skipMany1 (oneOf " \t\b")

-- | Parse a Prefix
prefix :: CharParser st Prefix
prefix  = char ':' >> (try nicknamePrefix <|> serverPrefix)

-- | Parse a Server prefix
serverPrefix :: CharParser st Prefix
serverPrefix  = takeUntil " " >>= return . Server

-- | Parse a NickName prefix
nicknamePrefix :: CharParser st Prefix
nicknamePrefix  = do
  n <- takeUntil " .!@\r\n"
  p <- option False (char '.' >> return True)
  when p (fail "")
  u <- optionMaybe $ char '!' >> takeUntil " @\r\n"
  s <- optionMaybe $ char '@' >> takeUntil " \r\n"
  return $ NickName n u s

-- | Parse a command.  Either a string of capital letters, or 3 digits.
command :: CharParser st Command
command  = (many1 upper)
        <|> do x <- digit
               y <- digit
               z <- digit
               return [x,y,z]

-- | Parse a command parameter.
parameter :: CharParser st Parameter
parameter  =  (char ':' >> takeUntil "\r\n")
          <|> (takeUntil " \r\n")

-- | Parse a cr lf
crlf :: CharParser st ()
crlf  =  (char '\r' >> optional (char '\n'))
     <|> (char '\n' >> return ()           )


-- | Parse a Message
message :: CharParser st Message
message  = do
  p <- optionMaybe $ tokenize prefix
  c <- command
  ps <- many (spaces >> parameter)
  crlf >> eof
  return $ Message p c ps
