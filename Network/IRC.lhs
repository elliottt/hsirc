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

> module Network.IRC
>   ( Parameter
>   , ServerName
>   , UserName
>   , Command
>   , Prefix(Server, NickName)
>   , Message(Message)

>   , parseMessage -- :: String -> Maybe Message
>   , formatMessage -- :: Message -> String
>   ) where

-------------------------------------------------------------------------------
-- Imports                                                                   --
-------------------------------------------------------------------------------

> import Control.Monad
> import Data.Maybe
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
> formatArgs  = unwords . formatArgs'

> formatArgs' :: [Parameter] -> [String]
> formatArgs' []                    = []
> formatArgs' l@(p:ps) | elem ' ' p = [":" ++ unwords l]
>                      | otherwise  = p : formatArgs' ps
