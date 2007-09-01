module Network.IRC.Client
  ( Client(Client)
  , runClient
  , write
  , joinChannel
  , user
  , nick
  )
  where

--------------------------------------------------------------------------------
-- Imports                                                                    --
--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Network.IRC.Parser
import System.IO

-- | Client is an IRC client type.  It holds state, as well as a Handle for
-- | communication back to the server.
newtype Client s a = Client (StateT s (ReaderT Handle IO) a)
                   deriving (Monad, MonadIO, MonadReader Handle, MonadState s)

io :: IO a -> Client s a
io  = liftIO

-- | Run a client.
runClient :: Client s a   -- | Client
             -> s         -- | Initial state
             -> Handle    -- | Network socket
             -> IO (a, s)
runClient (Client c) s h = c `runStateT` s `runReaderT` h

-- | Get the socket out of the monad to use
socket :: Client s Handle
socket  = ask

-- | Send a message to the server that the client is connected to
write  :: Message -> Client s ()
write m = do
  sock <- socket
  io $ hPutStr sock (formatMessage m ++ "\r\n")

-- | Join a channel
joinChannel  :: String -> Client s ()
joinChannel c = write (Message Nothing "JOIN" [c])

-- | Register a nickname
nick  :: UserName -> Client s ()
nick u = write (Message Nothing "NICK" [u])

-- | Register as a user
-- | The arguments are:
user        :: UserName      -- | Nickname
               -> ServerName -- | Host name
               -> ServerName -- | Server name
               -> RealName   -- | Real name
               -> Client s ()
user u h s r = write (Message Nothing "USER" [u,h,s,r])

-- | Read a message from the server
message :: Client s (Maybe Message)
message  = do
  l <- socket >>= liftIO . hGetLine
  return . parseMessage $ takeWhile (/= '\r') l
