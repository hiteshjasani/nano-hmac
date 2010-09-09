
---------------------------------------------------------------
-- |
-- Module      : Data.Digest.OpenSSL.HMAC
-- Copyright   : (c) Hitesh Jasani, 2008
-- License     : BSD3
--
-- Maintainer  : Hitesh Jasani <hitesh.jasani@gmail.com>
-- Stability   : experimental
-- Portability : requires FFI
--
-- Created     : 2008-02-03
--
-- Bindings to OpenSSL HMAC.
--
-- Sample Usage:
--
-- > d <- hmac md5 myKey myMessage
-- > putStrLn d
-- >
-- > "e9139d1e6ee064ef8cf514fc7dc83e86"
--
---------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Digest.OpenSSL.HMAC
    (
      hmac
    , unsafeHMAC
    , CryptoHashFunction
    , md5
    , sha, sha1, sha224, sha256, sha384, sha512
    ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Foreign
import Foreign.C.Types
import Numeric (showHex)

#include "openssl/hmac.h"
#include "openssl/md5.h"
#include "openssl/sha.h"

type OSSL_EVP_MD = Ptr ()

data CryptoHashFunction = CHF OSSL_EVP_MD Int

md5 :: CryptoHashFunction
md5 = CHF c_md5 (#const MD5_DIGEST_LENGTH)
sha :: CryptoHashFunction
sha = CHF c_sha (#const SHA_DIGEST_LENGTH)
sha1 :: CryptoHashFunction
sha1 = CHF c_sha1 (#const SHA_DIGEST_LENGTH)
sha224 :: CryptoHashFunction
sha224 = CHF c_sha224 (#const SHA224_DIGEST_LENGTH)
sha256 :: CryptoHashFunction
sha256 = CHF c_sha256 (#const SHA256_DIGEST_LENGTH)
sha384 :: CryptoHashFunction
sha384 = CHF c_sha384 (#const SHA384_DIGEST_LENGTH)
sha512 :: CryptoHashFunction
sha512 = CHF c_sha512 (#const SHA512_DIGEST_LENGTH)


{-|
  Generate an HMAC

  This implementation is safe and will copy the ByteStrings.

-}
hmac
 :: CryptoHashFunction          -- ^ hashing function
 -> B.ByteString                -- ^ key
 -> B.ByteString                -- ^ message
 -> IO String                   -- ^ resulting HMAC
hmac hf k p = return (unsafeHMAC hf (B.copy k) (B.copy p))


{-|
  Generate an HMAC

  This implementation is will not copy the ByteStrings and uses unsafePerformIO
-}
unsafeHMAC
 :: CryptoHashFunction          -- ^ hashing function
 -> B.ByteString                -- ^ key
 -> B.ByteString                -- ^ message
 -> String                      -- ^ resulting HMAC
unsafeHMAC (CHF evp_md len) k p =
  unsafePerformIO $
  BU.unsafeUseAsCStringLen k $ \(ptrKey,nKey) ->
  BU.unsafeUseAsCStringLen p $ \(ptr,n) -> do
    digest <- c_hmac evp_md ptrKey (fromIntegral nKey)
                     (castPtr ptr) (fromIntegral n) nullPtr nullPtr
    go digest 0 []
    {-
       The following code is inspired by (and mostly copied from) Don Stewart's
       nano-md5 library.  It worked and was almost exactly what I needed ...
       what else was I going to do?
    -}
    where
      go :: (Storable a, Integral a) => Ptr a -> Int -> [String] -> IO String
      go !q !n acc
          | n >= len  = return $ concat (reverse acc)
          | otherwise = do w <- peekElemOff q n
                           go q (n+1) (draw w : acc)

      draw :: (Integral a) => a -> String
      draw w = case showHex w [] of
                 [x] -> ['0', x]
                 x   -> x



foreign import ccall "openssl/hmac.h HMAC" c_hmac :: OSSL_EVP_MD -> Ptr CChar -> CInt -> Ptr Word8 -> CSize -> Ptr CUChar -> Ptr CUInt -> IO (Ptr Word8)

foreign import ccall "openssl/evp.h EVP_md5" c_md5 :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha" c_sha :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha1" c_sha1 :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha224" c_sha224 :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha256" c_sha256 :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha384" c_sha384 :: OSSL_EVP_MD
foreign import ccall "openssl/evp.h EVP_sha512" c_sha512 :: OSSL_EVP_MD

{-

unsigned char *HMAC(const EVP_MD *evp_md, const void *key, int key_len,
		    const unsigned char *d, size_t n, unsigned char *md,
		    unsigned int *md_len);

-}
