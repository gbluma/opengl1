.PHONY: build prod test

build:
	cabal configure
	cabal build
	# ghc *.hs -H100m -O2 -o opengl1 -hidir ./build -odir ./build 

prod:
	make build
	strip dist/build/opengl1/opengl1

	#strip opengl1

clean:
	cabal clean
	#rm ./build/* ./opengl1

test:
	make build
	./dist/build/opengl1/opengl1
