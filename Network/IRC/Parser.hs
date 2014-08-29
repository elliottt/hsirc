-- | Parsec parsers and a general parsing interface for IRC messages
module Network.IRC.Parser (
    -- * Parsing and Formatting Functions
    decode -- :: String -> Maybe Message

    -- * Parsec Combinators for Parsing IRC messages
  , prefix         -- :: Parser Prefix
  , serverPrefix   -- :: Parser Prefix
  , nicknamePrefix -- :: Parser Prefix
  , command        -- :: Parser Command
  , parameter      -- :: Parser Parameter
  , message        -- :: Parser Message
  , crlf           -- :: Parser ()
  , spaces         -- :: Parser ()

    -- * Deprecated Functions
  , parseMessage
  ) where

import Network.IRC.Base

import Data.Char
import Data.Word
import Data.ByteString hiding (elem, map, empty)

import Control.Monad (void)
import Control.Applicative
import Data.Attoparsec.ByteString

-- | Casts a character (assumed to be ASCII) to its corresponding byte.
asciiToWord8 :: Char -> Word8
asciiToWord8 = fromIntegral . ord

wSpace :: Word8
wSpace = asciiToWord8 ' '

wTab :: Word8
wTab = asciiToWord8 '\t'

wBell :: Word8
wBell = asciiToWord8 '\b'

wDot :: Word8
wDot = asciiToWord8 '.'

wExcl :: Word8
wExcl = asciiToWord8 '!'

wAt :: Word8
wAt = asciiToWord8 '@'

wCR :: Word8
wCR = asciiToWord8 '\r'

wLF :: Word8
wLF = asciiToWord8 '\n'

wColon :: Word8
wColon = asciiToWord8 ':'

-- | Parse a String into a Message.
decode :: ByteString    -- ^ Message string
       -> Maybe Message -- ^ Parsed message
decode str = case parseOnly message str of
  Left _ -> Nothing
  Right r -> Just r

-- | The deprecated version of decode
parseMessage :: ByteString -> Maybe Message
parseMessage  = decode

-- | Convert a parser that consumes all space after it
tokenize  :: Parser a -> Parser a
tokenize p = p <* spaces

-- | Consume only spaces, tabs, or the bell character
spaces :: Parser ()
spaces  = skip (\w -> w == wSpace ||
                      w == wTab ||
                      w == wBell)

-- | Parse a Prefix
prefix :: Parser Prefix
prefix  = word8 wColon *> (try nicknamePrefix <|> serverPrefix)

-- | Parse a Server prefix
serverPrefix :: Parser Prefix
serverPrefix  = Server <$> takeTill (== wSpace)

-- | optionMaybe p tries to apply parser p. If p fails without consuming input,
-- | it return Nothing, otherwise it returns Just the value returned by p.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | Parse a NickName prefix
nicknamePrefix :: Parser Prefix
nicknamePrefix  = do
  n <- takeTill (inClass " .!@\r\n")
  p <- peekWord8
  case p of
    Just c | c == wDot -> empty
    _                  -> NickName n <$>
                                optionMaybe (word8 wExcl *> takeTill (\w -> w == wSpace ||
                                                                            w == wAt ||
                                                                            w == wCR ||
                                                                            w == wLF))
                            <*> optionMaybe (word8 wAt *> takeTill (\w -> w == wSpace ||
                                                                          w == wCR ||
                                                                          w == wLF))

isWordAsciiUpper :: Word8 -> Bool
isWordAsciiUpper w = asciiToWord8 'A' <= w && w <= asciiToWord8 'Z'

digit :: Parser Word8
digit = satisfy (\w -> asciiToWord8 '0' <= w && w <= asciiToWord8 '9')

-- | Parse a command.  Either a string of capital letters, or 3 digits.
command :: Parser Command
command  = takeWhile1 isWordAsciiUpper
        <|> digitsToByteString <$>
                   digit
               <*> digit
               <*> digit
    where digitsToByteString x y z = pack [x,y,z]

-- | Parse a command parameter.
parameter :: Parser Parameter
parameter  =  (word8 wColon *> takeTill (\w -> w == wCR ||
                                               w == wLF))
          <|> takeTill (\w -> w == wSpace ||
                              w == wCR ||
                              w == wLF)

-- | Parse a cr lf
crlf :: Parser ()
crlf =  void (word8 wCR *> optional (word8 wLF))
    <|> void (word8 wLF)

-- | Parse a Message
message :: Parser Message
message  = Message <$>
      optionMaybe (tokenize prefix)
  <*> command
  <*> many (spaces *> parameter)
  <*  optional crlf
  <*  endOfInput
