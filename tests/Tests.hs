module Main where

-- Friends
import Network.IRC

-- Libraries
import Control.Applicative
import Control.Monad
import System.Random
import Test.QuickCheck
import Test.HUnit


instance Applicative Gen where
  (<*>) = ap
  pure  = return

-- ---------------------------------------------------------
-- Helpful Wrappers

-- An identifier starts with a letter, and consists of interspersed numbers
-- and special characters
newtype Identifier = Identifier { unIdentifier :: String }
  deriving (Read,Show,Eq)

instance Arbitrary Identifier where
  arbitrary   = do
      l  <- letter
      ls <- sized $ \n -> loop n
      return $ Identifier (l:ls)
    where loop n | n <= 0    = return []
                 | otherwise = do i  <- identifier
                                  is <- loop (n-1)
                                  return (i:is)

-- A hostname is a string that starts and ends with an identifier, and has
-- periods peppered in the middle.
newtype Host = Host { unHost :: String }

instance Arbitrary Host where
  arbitrary   = do
      l  <- identifier
      ls <- sized $ \n -> loop n
      js <- sized $ \n -> loop n
      e  <- identifier
      return $ Host (l:ls ++ ('.':js) ++ [e])
    where loop n | n <= 0    = return []
                 | otherwise = do i  <- host
                                  is <- loop (n-1)
                                  return (i:is)


letter :: Gen Char
letter  = frequency
  [ (50, choose ('a','z'))
  , (50, choose ('A','Z'))
  ]

digit :: Gen Char
digit  = choose ('0','9')

special :: Gen Char
special  = elements ['_','-']

identifier :: Gen Char
identifier  = frequency
  [ (50, letter)
  , (30, digit)
  , (10, special)
  ]

host :: Gen Char
host  = frequency
  [ (90, identifier)
  , (20, return '.')
  ]

-- ---------------------------------------------------------
-- IRC Types

newtype Cmd = Cmd { unCmd :: String }
  deriving (Read,Show,Eq)

instance Arbitrary Cmd where
  arbitrary   =
      let c = (replyTable !!) <$> choose (0, length replyTable - 1)
       in Cmd . fst <$> c


instance Arbitrary Prefix where
  arbitrary   = oneof
      [ do name <- unIdentifier <$> arbitrary
           user <- (liftM unIdentifier) <$> arbitrary
           host <- (liftM unIdentifier) <$> arbitrary
           return $ NickName name user host
      , do host <- unHost <$> arbitrary
           return $ Server host
      ]


instance Arbitrary Message where
  arbitrary   =
      let params = map unIdentifier <$> sized vector
          cmd    = unCmd <$> arbitrary
       in Message <$> arbitrary <*> cmd <*> params


-- ---------------------------------------------------------
-- Properties

prop_ircId    :: Message -> Bool
prop_ircId msg = (decode . (++ "\r\n") . encode $ msg) == Just msg


-- ---------------------------------------------------------
-- Unit Tests

tests :: Test
tests  = TestList $ map TestCase
  -- Initial colon encoding tests
  [ encode (Message Nothing "PRIVMSG" ["#foo", ":bar bas"]) @?=
    "PRIVMSG #foo ::bar bas"
  , encode (Message Nothing "PRIVMSG" ["#foo", ":bar"]) @?=
    "PRIVMSG #foo ::bar"

  -- Corrected case
  , decode ":talon.nl.eu.SwiftIRC.net 332 foo #bar :\n" @?=
    Just (Message (Just $ Server "talon.nl.eu.SwiftIRC.net")
            "332" ["foo","#bar",""])
  ]


-- ---------------------------------------------------------
-- Test Running

header :: String -> IO ()
header s = putStrLn "" >> putStrLn s >> putStrLn (replicate 60 '*')

main :: IO Counts
main  = do
  header "Checking irc encode/decode identity"
  quickCheck prop_ircId

  header "Checking individual test cases"
  runTestTT tests
