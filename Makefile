.PHONY: build prod test

build:
	ghc src/*.hs -H100m -O2 -o opengl1 -hidir ./build -odir ./build 

prod:
	make build
	strip opengl1

clean:
	rm ./build/* ./opengl1

test:
	make build
	./opengl1
