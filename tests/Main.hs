{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.IRC
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)

import Test.HUnit
import Test.QuickCheck

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

instance Arbitrary Prefix where
    arbitrary = do
        n <- choose (0, 1) :: Gen Int
        case n of
            0 -> do
                    let e = ['a'..'z']
                        e' = e ++ ['0'..'9'] ++ ['-', '.']
                    s <- elements e
                    s' <- listOf (elements e')
                    s'' <- elements e
                    return $ Server (pack (s : '.' : s' ++ [s'']))
            _ -> do
                    let e = ['a'..'z'] ++ ['A'..'Z']
                        e' = e ++ ['0'..'9'] ++ ['_', '^']
                        e'' = e ++ ['-', '.']
                    s <- elements e
                    s' <- vectorOf 8 (elements e')

                    n' <- choose (0, 1) :: Gen Int
                    t <- case n' of
                            0 -> return Nothing
                            _ -> do
                                    t' <- listOf1 (elements e'')
                                    return $ Just $ pack t'

                    n'' <- choose (0, 1) :: Gen Int
                    u <- case n'' of
                            0 -> return Nothing
                            _ -> do
                                    u' <- listOf1 (elements e'')
                                    return $ Just $ pack u'

                    return $ NickName (pack (s : s')) t u

instance Arbitrary Message where
    arbitrary = do
        p <- generatePrefix
        c <- generateCommand
        ps <- generateParams
        return (Message p c ps)

        where
            generatePrefix = do
                n <- choose (0, 1) :: Gen Int
                case n of
                    0 -> return Nothing
                    _ -> arbitrary

            generateCommand = do
                n <- choose (0, 1) :: Gen Int
                case n of
                    0 -> do
                            x <- choose (0, 9) :: Gen Int
                            y <- choose (0, 9) :: Gen Int
                            z <- choose (0, 9) :: Gen Int
                            return $ pack $ foldl (\a b -> a ++ (show b)) "" [x, y, z]
                    _ -> do
                            c <- listOf1 (elements ['A'..'Z'])
                            return $ pack c

            generateParams = do
                let e = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ",.+!\"§$%&/()=?_;'*"
                n <- choose (0, 13) :: Gen Int
                p <- vectorOf n $ listOf1 (elements e)

                n' <- choose (0, 1) :: Gen Int
                p' <- case n' of
                        0 -> return []
                        _ -> do
                                p'' <- listOf1 (elements (e ++ ": "))
                                return [p'']

                return $ map pack (p ++ p')


prop_encodeDecode :: Message -> Bool
prop_encodeDecode m = (fromJust . decode . encode) m == m

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Network.IRC" [
                testProperty "encodeDecode" prop_encodeDecode
                ],
        testGroup "HUnit tests Network.IRC" [
                testCase "PRIVMSG foo :bar baz" (decode "PRIVMSG foo :bar baz" @=? Just (Message Nothing "PRIVMSG" ["foo", "bar baz"])),
                testCase ":foo.bar NOTICE baz baz :baz baz" (decode ":foo.bar NOTICE baz baz :baz baz" @=? Just (Message (Just (Server "foo.bar")) "NOTICE" ["baz", "baz", "baz baz"])),
                testCase ":foo.bar 001 baz baz :baz baz" (decode ":foo.bar 001 baz baz :baz baz" @=? Just (Message (Just (Server "foo.bar")) "001" ["baz", "baz", "baz baz"])),
                testCase ":foo!bar@baz PRIVMSG #foo :bar baz" (decode ":foo!bar@baz PRIVMSG #foo :bar baz" @=? Just (Message (Just (NickName "foo" (Just "bar") (Just "baz"))) "PRIVMSG" ["#foo", "bar baz"])),
                testCase ":foo@baz PRIVMSG #foo :bar baz" (decode ":foo@baz PRIVMSG #foo :bar baz" @=? Just (Message (Just (NickName "foo" Nothing (Just "baz"))) "PRIVMSG" ["#foo", "bar baz"])),
                testCase ":foo!bar PRIVMSG #foo :bar baz" (decode ":foo!bar PRIVMSG #foo :bar baz" @=? Just (Message (Just (NickName "foo" (Just "bar") Nothing)) "PRIVMSG" ["#foo", "bar baz"])),
                testCase ":foo PRIVMSG #foo :bar baz" (decode ":foo PRIVMSG #foo :bar baz" @=? Just (Message (Just (NickName "foo" Nothing Nothing)) "PRIVMSG" ["#foo", "bar baz"]))
                ]
       ]
