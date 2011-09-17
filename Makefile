
all:
	ghc *.hs -o opengl1

clean:
	rm *.o *.hi ./opengl1

run:
	./opengl1
