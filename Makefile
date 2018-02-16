SRC=sudoku-solver

all:
	ghc --make $(SRC).hs

clean:
	rm $(SRC).hi $(SRC).o $(SRC)
