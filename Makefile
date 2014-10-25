
reload: clean build
	$(RM) *.hi *.o

build: clean
	mkdir bin
	ghc Main.hs -o bin/main

clean:
	rm -rf bin
	$(RM) *.hi *.o
