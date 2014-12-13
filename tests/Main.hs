{-# LANGUAGE OverloadedStrings #-}
module Main
    (
      main
    ) where

import Network.IRC

import Data.ByteString (ByteString, append, pack)
import Data.Word (Word8)
import Data.Char (ord)

import Control.Applicative ((<$>), (<*>), liftA)

import Test.HUnit
import Test.QuickCheck

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- ---------------------------------------------------------
-- Helpful Wrappers

-- An identifier starts with a letter, and consists of interspersed numbers
-- and special characters
newtype Identifier = Identifier { unIdentifier :: ByteString }
  deriving (Read,Show,Eq)

instance Arbitrary Identifier where
  arbitrary   = do
      l  <- letter
      ls <- sized $ \n -> loop n
      return $ Identifier (pack (l:ls))
    where loop n | n <= 0    = return []
                 | otherwise = do i  <- identifier
                                  is <- loop (n-1)
                                  return (i:is)

-- A hostname is a string that starts and ends with an identifier, and has
-- periods peppered in the middle.
newtype Host = Host { unHost :: ByteString }

instance Arbitrary Host where
  arbitrary   = do
      l  <- identifier
      ls <- sized $ \n -> loop n
      js <- sized $ \n -> loop n
      e  <- identifier
      return $ Host (pack (l:ls ++ (w8 '.':js) ++ [e]))
    where loop n | n <= 0    = return []
                 | otherwise = do i  <- host
                                  is <- loop (n-1)
                                  return (i:is)


w8 :: Char -> Word8
w8 = fromIntegral . ord

letter :: Gen Word8
letter  = frequency
  [ (50, choose (w8 'a', w8 'z'))
  , (50, choose (w8 'A', w8 'Z'))
  ]

digit :: Gen Word8
digit  = choose (w8 '0', w8 '9')

special :: Gen Word8
special  = elements [w8 '_', w8 '-']

identifier :: Gen Word8
identifier  = frequency
  [ (50, letter)
  , (30, digit)
  , (10, special)
  ]

host :: Gen Word8
host  = frequency
  [ (90, identifier)
  , (20, return (w8 '.'))
  ]

-- ---------------------------------------------------------
-- IRC Types

newtype Cmd = Cmd { unCmd :: ByteString }
  deriving (Read,Show,Eq)

instance Arbitrary Cmd where
  arbitrary   =
      let c = (replyTable !!) <$> choose (0, length replyTable - 1)
       in Cmd . fst <$> c

instance Arbitrary Prefix where
  arbitrary   = oneof
      [ NickName
            <$> fmap unIdentifier arbitrary
            <*> fmap (liftA unIdentifier) arbitrary
            <*> fmap (liftA unIdentifier) arbitrary
      , Server
            <$> fmap unHost arbitrary
      ]

instance Arbitrary Message where
  arbitrary   =
      let params = map unIdentifier <$> sized vector
          cmd    = unCmd <$> arbitrary
       in Message <$> arbitrary <*> cmd <*> params

-- ---------------------------------------------------------
-- Properties

prop_encodeDecode    :: Message -> Bool
prop_encodeDecode msg = (decode . appendCRLF . encode $ msg)
                     == Just msg
  where appendCRLF bs = append bs (pack [w8 '\r', w8 '\n'])

properties :: TF.Test
properties = testGroup "QuickCheck Network.IRC"
    [ testProperty "encodeDecode" prop_encodeDecode
    ]

-- ---------------------------------------------------------
-- Unit Tests

unitTests :: TF.Test
unitTests = testGroup "HUnit tests Network.IRC"
    [ -- Decoding tests
      testCase "PRIVMSG foo :bar baz"
               (    decode "PRIVMSG foo :bar baz"
                @=? Just (Message Nothing "PRIVMSG" ["foo", "bar baz"]))
    , testCase ":foo.bar NOTICE baz baz :baz baz"
               (    decode ":foo.bar NOTICE baz baz :baz baz"
                @=? Just (Message (Just (Server "foo.bar")) "NOTICE" ["baz", "baz", "baz baz"]))
    , testCase ":foo.bar 001 baz baz :baz baz"
               (    decode ":foo.bar 001 baz baz :baz baz"
                @=? Just (Message (Just (Server "foo.bar")) "001" ["baz", "baz", "baz baz"]))
    , testCase ":foo!bar@baz PRIVMSG #foo :bar baz"
               (    decode ":foo!bar@baz PRIVMSG #foo :bar baz"
                @=? Just (Message (Just (NickName "foo" (Just "bar") (Just "baz"))) "PRIVMSG" ["#foo", "bar baz"]))
    , testCase ":foo@baz PRIVMSG #foo :bar baz"
               (    decode ":foo@baz PRIVMSG #foo :bar baz"
                @=? Just (Message (Just (NickName "foo" Nothing (Just "baz"))) "PRIVMSG" ["#foo", "bar baz"]))
    , testCase ":foo!bar PRIVMSG #foo :bar baz"
               (    decode ":foo!bar PRIVMSG #foo :bar baz"
                @=? Just (Message (Just (NickName "foo" (Just "bar") Nothing)) "PRIVMSG" ["#foo", "bar baz"]))
    , testCase ":foo PRIVMSG #foo :bar baz"
               (    decode ":foo PRIVMSG #foo :bar baz"
                @=? Just (Message (Just (NickName "foo" Nothing Nothing)) "PRIVMSG" ["#foo", "bar baz"]))

      -- Decoding tests

      -- Initial colon encoding tests
    , testCase "Message Nothing \"PRIVMSG\" [\"#foo\", \":bar bas\"]"
               (    encode (Message Nothing "PRIVMSG" ["#foo", ":bar bas"])
                @?= "PRIVMSG #foo ::bar bas")
    , testCase "Message Nothing \"PRIVMSG\" [\"#foo\", \":bar\"]"
               (    encode (Message Nothing "PRIVMSG" ["#foo", ":bar"])
                @?= "PRIVMSG #foo ::bar")

    -- Corrected case
    , testCase ":talon.nl.eu.SwiftIRC.net 332 foo #bar :\n"
               (    decode ":talon.nl.eu.SwiftIRC.net 332 foo #bar :\n"
                @?= Just (Message (Just $ Server "talon.nl.eu.SwiftIRC.net") "332" ["foo","#bar",""]))
    ]

-- ---------------------------------------------------------
-- Test List

tests :: [TF.Test]
tests = [ properties
        , unitTests
        ]

main :: IO ()
main = defaultMain tests

