
package:
	runhaskell Setup.lhs sdist

compile:
	hsc2hs Data/Digest/OpenSSL/HMAC.hsc

publish-doc:
	echo "copy the doc files to the hosts-trunk/Grokken1/nginx/docs.jasani.org"
	echo "tree."

clean:
	-rm Data/Digest/OpenSSL/HMAC.hs
	-find . -name '*.o' -print | xargs rm
	-find . -name '*.hi' -print | xargs rm
	-find . -name '*~' -print | xargs rm
