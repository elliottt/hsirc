module Network.IRC.Monad
  ( IRC(IRC)
  , runIRC
  , write
  , joinChannel
  , user
  , nick
  , quit
  , part
  , message
  , messages
  )
  where

--------------------------------------------------------------------------------
-- Imports                                                                    --
--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader
import Network.IRC.Parser
import System.IO

-- | The IRC monad wraps up the handle to allow for a simpler interface to the
-- | socket.
newtype IRC a = IRC (ReaderT Handle IO a)
                   deriving (Monad, MonadIO, MonadReader Handle)

io :: IO a -> IRC a
io  = liftIO

-- | Run an irc.
runIRC :: IRC a   -- | IRC
             -> Handle    -- | Network socket
             -> IO a
runIRC (IRC c) h = c `runReaderT` h

-- | Get the socket out of the monad to use
socket :: IRC Handle
socket  = ask

-- | Send a message to the server that the client is connected to
write  :: Message -> IRC ()
write m = socket >>= \s -> io $ hPutStr s (formatMessage m ++ "\r\n")

-- | Join a channel
joinChannel   :: [(String,String)] -> IRC ()
joinChannel cs = write $ Message Nothing "JOIN" $ (uncurry (++) . unzip) cs

-- | Register a nickname
nick  :: UserName -> IRC ()
nick u = write (Message Nothing "NICK" [u])

-- | Register as a user
-- | The arguments are:
user        :: UserName      -- | Nickname
               -> ServerName -- | Host name
               -> ServerName -- | Server name
               -> RealName   -- | Real name
               -> IRC ()
user u h s r = write (Message Nothing "USER" [u,h,s,r])

-- | Leave the server
quit :: IRC ()
quit  = write (Message Nothing "QUIT" [])

-- | Leave a channel
part :: String         -- | Message
        -> [String]    -- | Channels
        -> IRC ()
part m cs = write (Message Nothing "PART" (cs ++ [m]))

-- | Read a message from the server
message :: IRC (Maybe Message)
message  = do
  l <- socket >>= io . hGetLine
  return . parseMessage $ takeWhile (/= '\r') l

-- | Generate a stream of messages from the server
messages :: IRC [Maybe Message]
messages  = do
  stream <- socket >>= io . hGetContents
  return . map (parseMessage . takeWhile (/='\r')) . lines $ stream
