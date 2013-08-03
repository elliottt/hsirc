module Network.IRC.Commands (
    -- * Types
    Channel
  , Password

    -- * IRC Functions
  , nick
  , user
  , joinChan
  , part
  , quit
  , privmsg
  , kick
  ) where

import Network.IRC.Base

type Channel    = String
type Password   = String
type Reason     = String

mkMessage           :: String -> [Parameter] -> Message
mkMessage cmd params = Message Nothing cmd params




nick  :: UserName -> Message
nick u = mkMessage "NICK" [u]

user        :: UserName -> ServerName -> ServerName -> RealName -> Message
user u h s r = mkMessage "USER" [u,h,s,r]

joinChan  :: Channel -> Message
joinChan c = mkMessage "JOIN" [c]

kick :: Channel -> UserName -> Maybe Reason -> Message
kick c u (Just r) = mkMessage "KICK" [c,u,r]
kick c u Nothing  = mkMessage "KICK" [c,u]

part  :: Channel -> Message
part c = mkMessage "PART" [c]

quit :: Maybe String -> Message
quit (Just m) = mkMessage "QUIT" [m]
quit Nothing  = mkMessage "QUIT" []

privmsg    :: String -> String -> Message
privmsg c m = mkMessage "PRIVMSG" [c,m]

pong  :: ServerName -> Message
pong s = mkMessage "PONG" [s]
