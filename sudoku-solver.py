import sys

class SudokuSolverException(Exception):
    pass

def usage():
    print "usage: $ python " + sys.argv[0] + " <sudokuFile>"
    print "  - sudokuFile contains sudoku such as:"
    print "    +-+"
    print "    |1|"
    print "    +-+"
    print "  - or:"
    print "    +--+--+"
    print "    | 2|34|"
    print "    |43| 1|"
    print "    +--+--+"
    print "    |  | 2|"
    print "    |24|13|"
    print "    +--+--+"
    print "  - or:"
    print "    +---+---+---+"
    print "    |   | 5 |49 |"
    print "    |214|396|785|"
    print "    | 5 |4  |63 |"
    print "    +---+---+---+"
    print "    |   |6  | 18|"
    print "    | 95|7 8|26 |"
    print "    |86 |   |  7|"
    print "    +---+---+---+"
    print "    | 36| 71|   |"
    print "    |5  |  3| 76|"
    print "    | 78| 6 |   |"
    print "    +---+---+---+"
    print "  - etc..."
    print "  - footnote - sudoku may be of size:"
    print "      - 1 (a little boring),"
    print "      - 2 (still somewhat boring),"
    print "      - 3 (customary)."

def calculateSudokuSize(firstLine):
    pass
    #count occurences of '-' and return its sqrt

def validateSudokuFile(sudokuFile):
    content = sudokuFile.readlines()
    sudokuSize = calculateSudokuSize(content[0])

def main():
    if (1 == len(sys.argv)):
        usage()
    else:
        sudokuFile = open(sys.argv[1], "r")
        validateSudokuFile(sudokuFile)


if __name__ == "__main__":
    main()

