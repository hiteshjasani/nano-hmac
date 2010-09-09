---------------------------------------------------------------
-- |
-- Module      : hmac_hu
-- Copyright   : (c) Hitesh Jasani, 2008
-- License     : BSD3
--
-- Maintainer  : Hitesh Jasani <hitesh.jasani@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Created     : 2008-02-06
-- Version     : 0.0.1.0
--
-- hunit tests -- see RFC 2104 and RFC 2202 for test vectors
--
---------------------------------------------------------------

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as BI
import Data.Digest.OpenSSL.HMAC
import Data.Word
import Test.HUnit


main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [ "md5 tests"         ~: hmac_tests safe_hmacs md5 md5_data
                 , "unsafe md5 tests"  ~: hmac_tests unsafe_hmacs md5 md5_data
                 , "sha1 tests"        ~: hmac_tests safe_hmacs sha1 sha1_data
                 , "unsafe sha1 tests" ~: hmac_tests unsafe_hmacs sha1 sha1_data
                 ]


safe_hmacs
 :: CryptoHashFunction          -- ^ hashing function
 -> [Word8]                     -- ^ key
 -> String                      -- ^ message
 -> String                      -- ^ expected digest
 -> Assertion
safe_hmacs hf k m d = do
  digest <- hmac hf (B.pack k) (B8.pack m)
  d @=? digest

unsafe_hmacs
 :: CryptoHashFunction          -- ^ hashing function
 -> [Word8]                     -- ^ key
 -> String                      -- ^ message
 -> String                      -- ^ expected digest
 -> Assertion
unsafe_hmacs hf k m d =
  d @=? unsafeHMAC hf (B.pack k) (B8.pack m)


md5_data :: [(String, [Word8], String, String)]
md5_data = [ ( "no key"
             , []
             , "More text test vectors to stuff up EBCDIC machines :-)"
             , "e9139d1e6ee064ef8cf514fc7dc83e86")

           , ( "binary 16 0x0b key"
             , (replicate 16 0x0b)
             , "Hi There"
             , "9294727a3638bb1c13f48ef8158bfc9d")

           , ( "ascii Jefe key"
             , (map BI.c2w "Jefe")
             , "what do ya want for nothing?"
             , "750c783e6ab0b503eaa86e310a5db738")

           , ( "binary 16 0xaa key, binary 50 0xdd msg"
             , (replicate 16 0xaa)
             , (map BI.w2c $ replicate 50 0xdd)
             , "56be34521d144c88dbb8c733f0e8b3f6")

--            , ( "binary 25 digit key, binary 50 0xcd msg"
--              , 0x0102030405060708090a0b0c0d0e0f10111213141516171819
--              , (map BI.w2c $ replicate 50 0xcd)
--              , "0x697eaf0aca3a3aea3a75164746ffaa79")

           , ( "binary 16 0x0c"
             , (replicate 16 0x0c)
             , "Test With Truncation"
             , "56461ef2342edc00f9bab995690efd4c")

           , ( "binary 80 0xaa"
             , (replicate 80 0xaa)
             , "Test Using Larger Than Block-Size Key - Hash Key First"
             , "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd")

           , ( "binary 80 0xaa and larger than block size data"
             , (replicate 80 0xaa)
             , "Test Using Larger Than Block-Size Key and Larger\n Than One Block-Size Data"
             , "6f630fad67cda0ee1fb1f562db3aa53e")
           ]


sha1_data :: [(String, [Word8], String, String)]
sha1_data = [ ( "binary 20 0x0b"
              , (replicate 20 0x0b)
              , "Hi There"
              , "b617318655057264e28bc0b6fb378c8ef146be00")

            , ( "ascii Jefe key"
              , (map BI.c2w "Jefe")
              , "what do ya want for nothing?"
              , "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")

            , ( "binary 20 0xaa"
              , (replicate 20 0xaa)
              , (map BI.w2c $ replicate 50 0xdd)
              , "125d7342b9ac11cd91a39af48aa17b4f63f175d3")

--            , ( "binary 25 digit key, binary 50 0xcd msg"
--              , (map BI.c2w "0x0102030405060708090a0b0c0d0e0f10111213141516171819")
--              , (map BI.w2c $ replicate 50 0xcd)
--              , "4c9007f4026250c6bc8414f9bf50c86c2d7235da")

            , ( "binary 20 0x0c"
              , (replicate 20 0x0c)
              , "Test With Truncation"
              , "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04")

            , ( "binary 80 0xaa"
              , (replicate 80 0xaa)
              , "Test Using Larger Than Block-Size Key - Hash Key First"
              , "aa4ae5e15272d00e95705637ce8a3b55ed402112")

            , ( "binary 80 0xaa and 73 char msg"
              , (replicate 80 0xaa)
              , "Test Using Larger Than Block-Size Key and Larger  Than One Block-Size Data"
              , "e8e99d0f45237d786d6bbaa7965c7808bbff1a91")
            ]


hmac_tests :: (Testable t) => (a -> b -> c -> d -> t) -> a -> [(String, b, c, d)] -> Test
hmac_tests thmf hf = TestList . map (\(d,k,m,e) -> d ~: thmf hf k m e)

