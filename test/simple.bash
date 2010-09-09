
function dmd5 { ghc -e "putStr \"d md5:  \" >> return (Data.Digest.OpenSSL.MD5.md5sum (Data.ByteString.Char8.pack ($*))) >>= putStrLn" -package nano-md5 -package bytestring -lcrypto -fvia-C -fffi -fbang-patterns; }

function hhmac { ghc -e "putStr \"h hmac: \" >> hmac (BC8.pack ($*)) >>= putStrLn" Data.Digest.OpenSSL.HMAC -i.. -package bytestring -lcrypto; }

echo "------"
echo "md5: /usr/share/dict/words"; openssl dgst -md5 /usr/share/dict/words
cat /usr/share/dict/words | dmd5

# echo "------"
# echo "md5: foobar"; echo -n "foobar" | openssl dgst -md5
# # ghc -e "return (Data.Digest.OpenSSL.MD5.md5sum (Data.ByteString.Char8.pack \"foobar\")) >>= putStrLn" -package nano-md5 -package bytestring -lcrypto -fvia-C -fffi -fbang-patterns
# dmd5 "foobar"
# # ghc -e "hmac (BC8.pack \"foobar\") >>= putStrLn" Data.Digest.OpenSSL.HMAC -i.. -lcrypto
# hhmac "foobar"

# echo "------"
# echo "md5: More text test vectors to stuff up EBCDIC machines :-)"; echo -n "More text test vectors to stuff up EBCDIC machines :-)" | openssl dgst -md5

# dmd5 "More text test vectors to stuff up EBCDIC machines :-)"
# hhmac "More text test vectors to stuff up EBCDIC machines :-)"

# echo "------"

# echo "sha1: "; echo -n "foobar" | openssl dgst -sha1

# echo "sha256: "; echo -n "foobar" | openssl dgst -sha256

# ghci Data.Digest.OpenSSL.HMAC -lcrypto

# hmac (BC8.pack "foobar")

