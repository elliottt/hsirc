-- |
-- Module      : Network.IRC
-- Copyright   : (c) Trevor Elliott 2007
-- License     : BSD3
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
