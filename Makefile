
all:
	ghc *.hs -H30m -O2 -o opengl1

prod:
	ghc *.hs -H30m -O2 -o opengl1
	strip opengl1

clean:
	rm *.o *.hi ./opengl1

run:
	./opengl1
