This file is part of irc.

irc is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

irc is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

| Module      : Network.IRC
| Copyright   : (c) Trevor Elliott 2007
| License     : LGPL
|
| Maintainer  : trevor@geekgateway.com
| Stability   : experimental
| Portability : non-portable
|
| A library for parsing IRC messages.

> module Network.IRC.Parser
>   ( Parameter
>   , ServerName
>   , UserName
>   , RealName
>   , Command
>   , Prefix(Server, NickName)
>   , Message(Message)

>   , parseMessage   -- :: String -> Maybe Message
>   , formatMessage  -- :: Message -> String
>   , translateReply -- :: String -> String
>   ) where

-------------------------------------------------------------------------------
-- Imports                                                                   --
-------------------------------------------------------------------------------

> import Control.Monad
> import Data.Maybe (maybe, fromMaybe)
> import Text.ParserCombinators.Parsec hiding (spaces)
> import Text.Printf

  IRC messages are of the format:

  [ ':' <prefix> ] <command> { <space> <param> } <crlf>

> data Message = Message (Maybe Prefix) Command [Parameter]
>              deriving (Show)

> type Command    = String
> type Parameter  = String
> type ServerName = String
> type UserName   = String
> type RealName   = String

| A prefix is the beginning of the message

> data Prefix  = Server ServerName
>              | NickName String (Maybe UserName) (Maybe ServerName)
>              deriving (Show)

| Parse a String into a Message.

> parseMessage :: String -> Maybe Message
> parseMessage  = (either (const Nothing) Just) . (parse message "")

> takeUntil :: String -> Parser String
> takeUntil  = many1 . noneOf

> maybeP  :: Parser a -> Parser (Maybe a)
> maybeP p = option Nothing (liftM Just p)

> tokenize  :: Parser a -> Parser a
> tokenize p = p >>= \x -> spaces >> return x

> spaces :: Parser ()
> spaces  = skipMany1 (char ' ')

> prefix :: Parser Prefix
> prefix  = char ':' >> (try nickname <|> server)

> server :: Parser Prefix
> server  = takeUntil " " >>= return . Server

> nickname :: Parser Prefix
> nickname  = do
>   n <- takeUntil " .!@"
>   p <- option False (char '.' >> return True)
>   when p (fail "")
>   u <- maybeP $ char '!' >> takeUntil " @"
>   s <- maybeP $ char '@' >> takeUntil " "
>   return $ NickName n u s

> command :: Parser Command
> command  = many1 upper
>         <|> do x <- digit
>                y <- digit
>                z <- digit
>                return [x,y,z]

> parameter :: Parser Parameter
> parameter  =  (char ':' >> takeUntil "\r\n")
>           <|> (takeUntil " \r\n")

> crlf :: Parser ()
> crlf  = string "\r\n" >> return ()

> message :: Parser Message
> message  = do
>   p <- maybeP $ tokenize prefix
>   c <- command
>   ps <- many (spaces >> parameter)
>   crlf <|> eof
>   return $ Message p c ps

| Message formatting

> formatMessage :: Message -> String
> formatMessage m@(Message p c args) =
>   (maybe "" (\p' -> formatPrefix p' ++ " ") p) ++ c ++ " "
>   ++ formatArgs args
>   

> formatPrefix :: Prefix -> String
> formatPrefix (Server n)       = ":" ++ n
> formatPrefix (NickName n u s) = ":" ++ n
>   ++ maybe "" (\u' -> "!" ++ u') u
>   ++ maybe "" (\s' -> "@" ++ s') s

> formatArgs :: [Parameter] -> String
> formatArgs  = unwords . formatArgs' . filter ((>0) . length)

> formatArgs' :: [Parameter] -> [String]
> formatArgs' []                    = []
> formatArgs' l@(p:ps) | elem ' ' p = [":" ++ unwords l]
>                      | otherwise  = p : formatArgs' ps

> translateReply :: String -> String
> translateReply r = fromMaybe r $ lookup r replyTable

> replyTable :: [(String,String)]
> replyTable  =
>   [ ("401","ERR_NOSUCHNICK")
>   , ("402","ERR_NOSUCHSERVER")
>   , ("403","ERR_NOSUCHCHANNEL")
>   , ("404","ERR_CANNOTSENDTOCHAN")
>   , ("405","ERR_TOOMANYCHANNELS")
>   , ("406","ERR_WASNOSUCHNICK")
>   , ("407","ERR_TOOMANYTARGETS")
>   , ("409","ERR_NOORIGIN")
>   , ("411","ERR_NORECIPIENT")
>   , ("412","ERR_NOTEXTTOSEND")
>   , ("413","ERR_NOTOPLEVEL")
>   , ("414","ERR_WILDTOPLEVEL")
>   , ("421","ERR_UNKNOWNCOMMAND")
>   , ("422","ERR_NOMOTD")
>   , ("423","ERR_NOADMININFO")
>   , ("424","ERR_FILEERROR")
>   , ("431","ERR_NONICKNAMEGIVEN")
>   , ("432","ERR_ERRONEUSNICKNAME")
>   , ("433","ERR_NICKNAMEINUSE")
>   , ("436","ERR_NICKCOLLISION")
>   , ("441","ERR_USERNOTINCHANNEL")
>   , ("442","ERR_NOTONCHANNEL")
>   , ("443","ERR_USERONCHANNEL")
>   , ("444","ERR_NOLOGIN")
>   , ("445","ERR_SUMMONDISABLED")
>   , ("446","ERR_USERSDISABLED")
>   , ("451","ERR_NOTREGISTERED")
>   , ("461","ERR_NEEDMOREPARAMS")
>   , ("462","ERR_ALREADYREGISTRED")
>   , ("463","ERR_NOPERMFORHOST")
>   , ("464","ERR_PASSWDMISMATCH")
>   , ("465","ERR_YOUREBANNEDCREEP")
>   , ("467","ERR_KEYSET")
>   , ("471","ERR_CHANNELISFULL")
>   , ("472","ERR_UNKNOWNMODE")
>   , ("473","ERR_INVITEONLYCHAN")
>   , ("474","ERR_BANNEDFROMCHAN")
>   , ("475","ERR_BADCHANNELKEY")
>   , ("481","ERR_NOPRIVILEGES")
>   , ("482","ERR_CHANOPRIVSNEEDED")
>   , ("483","ERR_CANTKILLSERVER")
>   , ("491","ERR_NOOPERHOST")
>   , ("501","ERR_UMODEUNKNOWNFLAG")
>   , ("502","ERR_USERSDONTMATCH")
>   , ("300","RPL_NONE")
>   , ("302","RPL_USERHOST")
>   , ("303","RPL_ISON")
>   , ("301","RPL_AWAY")
>   , ("305","RPL_UNAWAY")
>   , ("306","RPL_NOWAWAY")
>   , ("311","RPL_WHOISUSER")
>   , ("312","RPL_WHOISSERVER")
>   , ("313","RPL_WHOISOPERATOR")
>   , ("317","RPL_WHOISIDLE")
>   , ("318","RPL_ENDOFWHOIS")
>   , ("319","RPL_WHOISCHANNELS")
>   , ("314","RPL_WHOWASUSER")
>   , ("369","RPL_ENDOFWHOWAS")
>   , ("321","RPL_LISTSTART")
>   , ("322","RPL_LIST")
>   , ("323","RPL_LISTEND")
>   , ("324","RPL_CHANNELMODEIS")
>   , ("331","RPL_NOTOPIC")
>   , ("332","RPL_TOPIC")
>   , ("341","RPL_INVITING")
>   , ("342","RPL_SUMMONING")
>   , ("351","RPL_VERSION")
>   , ("352","RPL_WHOREPLY")
>   , ("315","RPL_ENDOFWHO")
>   , ("353","RPL_NAMREPLY")
>   , ("366","RPL_ENDOFNAMES")
>   , ("364","RPL_LINKS")
>   , ("365","RPL_ENDOFLINKS")
>   , ("367","RPL_BANLIST")
>   , ("368","RPL_ENDOFBANLIST")
>   , ("371","RPL_INFO")
>   , ("374","RPL_ENDOFINFO")
>   , ("375","RPL_MOTDSTART")
>   , ("372","RPL_MOTD")
>   , ("376","RPL_ENDOFMOTD")
>   , ("381","RPL_YOUREOPER")
>   , ("382","RPL_REHASHING")
>   , ("391","RPL_TIME")
>   , ("392","RPL_USERSSTART")
>   , ("393","RPL_USERS")
>   , ("394","RPL_ENDOFUSERS")
>   , ("395","RPL_NOUSERS")
>   , ("200","RPL_TRACELINK")
>   , ("201","RPL_TRACECONNECTING")
>   , ("202","RPL_TRACEHANDSHAKE")
>   , ("203","RPL_TRACEUNKNOWN")
>   , ("204","RPL_TRACEOPERATOR")
>   , ("205","RPL_TRACEUSER")
>   , ("206","RPL_TRACESERVER")
>   , ("208","RPL_TRACENEWTYPE")
>   , ("261","RPL_TRACELOG")
>   , ("211","RPL_STATSLINKINFO")
>   , ("212","RPL_STATSCOMMANDS")
>   , ("213","RPL_STATSCLINE")
>   , ("214","RPL_STATSNLINE")
>   , ("215","RPL_STATSILINE")
>   , ("216","RPL_STATSKLINE")
>   , ("218","RPL_STATSYLINE")
>   , ("219","RPL_ENDOFSTATS")
>   , ("241","RPL_STATSLLINE")
>   , ("242","RPL_STATSUPTIME")
>   , ("243","RPL_STATSOLINE")
>   , ("244","RPL_STATSHLINE")
>   , ("221","RPL_UMODEIS")
>   , ("251","RPL_LUSERCLIENT")
>   , ("252","RPL_LUSEROP")
>   , ("253","RPL_LUSERUNKNOWN")
>   , ("254","RPL_LUSERCHANNELS")
>   , ("255","RPL_LUSERME")
>   , ("256","RPL_ADMINME")
>   , ("257","RPL_ADMINLOC1")
>   , ("258","RPL_ADMINLOC2")
>   , ("259","RPL_ADMINEMAIL")
>   ]
