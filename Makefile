CC= ghc

all: haskinator

haskinator: 
	$(CC) haskinator --make
	rm -rf *.o *.hi

clean:
	rm -rf *.o *.hi *.exe
