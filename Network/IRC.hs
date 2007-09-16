-- This file is part of irc.

-- irc is free software; you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation; either version 3 of the License, or
-- (at your option) any later version.

-- irc is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- |
-- Module      : Network.IRC
-- Copyright   : (c) Trevor Elliott 2007
-- License     : LGPL
--
-- Maintainer  : trevor@geekgateway.com
-- Stability   : experimental
-- Portability : non-portable (GeneralizedNewtypeDeriving)
--
-- Library for parsing IRC messages

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.IRC (
    -- Re-export data and parser
    module Network.IRC.Datatypes
  , module Network.IRC.Parser

    -- IRC actions
  , simpleIRC
  , messages
  , pass
  , nick
  , user
  , joinChannel
  , part
  , privmsg

  , -- Channel actions
  ) where

-- Friends
import Network.IRC.Datatypes
import Network.IRC.Parser

-- Libs
import Control.Monad.Reader
import Data.Maybe
import System.IO

-- | IRC Monad.
-- Used for all operations that aren't specific to any particular context.
newtype IRC a = IRC { runIRC :: ReaderT Handle IO a }
              deriving (Monad, MonadReader Handle, MonadIO)

-- shortcut to liftIO
io :: IO a -> IRC a
io  = liftIO

-- shortcut for sending irc messages
msg :: Command -> [Parameter] -> IRC ()
msg c ps = simpleIRC $ Message Nothing c ps

-- | Run the IRC monad
irc :: IRC a -> Handle -> IO a
irc  = runReaderT . runIRC

-- | Write a formatted IRC message to a Handle
simpleIRC :: Message -- ^ The message to send
          -> IRC ()
simpleIRC m = ask >>= \s -> io . hPutStr s $ render m ++ "\r\n"

-- | Generate a stream of parsed IRC messages
messages :: IRC [ Maybe Message ]
messages  = ask
            >>= \s -> io (hGetContents s)
            >>= return . map (parseMessage . (++"\n")) . lines

-- | Send a password
pass :: String -- ^ Password
     -> IRC ()
pass p = msg "PASS" [p]

-- | Request a nickname
nick :: String -- ^ Nickname
     -> IRC ()
nick n = msg "NICK" [n]

-- | User request
user :: String -- User
     -> String -- Host name
     -> String -- Server name
     -> String -- Real name
     -> IRC ()
user u h s r = msg "USER" [u,h,s,r]

-- | Join a channel
joinChannel :: String       -- ^ Channel
            -> Maybe String -- ^ Password
            -> IRC ()
joinChannel c (Just p) = msg "JOIN" [c,p]
joinChannel c Nothing  = msg "JOIN" [c]

-- | Leave a channel
part :: String       -- ^ Channel
     -> Maybe String -- ^ Parting message
     -> IRC ()
part c (Just r) = msg "PART" [c,r]
part c Nothing  = msg "PART" [c]

-- | Send a message
privmsg :: String -- ^ Recipient
        -> String -- ^ Message
        -> IRC ()
privmsg t m = msg "PRIVMSG" [t,m]

-- | Mode message
mode :: (Prop p)
     => String       -- ^ Target (Channel or User)
     -> Mode p       -- ^ The mode to apply
     -> Maybe Int    -- ^ Limit
     -> Maybe String -- ^ User to target
     -> Maybe String -- ^ Ban mask
     -> IRC ()
mode t m l u b = msg "MODE" ([t, renderMode m] ++ catMaybes [fmap show l,u,b])
