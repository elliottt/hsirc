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
-- Portability : non-portable
--
-- library for parsing IRC messages
--
module Network.IRC (
    -- * Parsers
    module Network.IRC.Parser

    -- * Base
  , module Network.IRC.Base
  
    -- * Message API
  , module Network.IRC.Commands
  ) where

import Network.IRC.Parser
import Network.IRC.Base
import Network.IRC.Commands
