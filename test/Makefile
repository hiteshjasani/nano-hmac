
hmac_hu: hmac_hu.hs
	ghc --make -Wall -Werror $< -o $@ -O2 -i.. -lcrypto -lssl -fvia-C -fffi -fbang-patterns -cpp -L/opt/local/lib

clean:
	-rm hmac_hu *~ *.hi *.o
