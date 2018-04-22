"""A translator from unbounded-cell brainfuck with numeric I/O to
Number Factory."""

import sys

# Snippet to be placed at the start of the program.
START_SNIPPET = "V<<%>V%"

INC_SNIPPET = "%V<%>^^%VV<%%>^%"
DEC_SNIPPET = "%<%>^%V<%%>%"

# Causes the robot to pick up 0 if it has nothing, assuming that it
# is on S. This works by having it attempt to place its content in -
# and then placing 0 in -, then immediately taking from - again.
OR_ZERO = "<%^%V%%>"

LEFT_SNIPPET = "%>%<%" + OR_ZERO + "%"
RIGHT_SNIPPET = ">%<" + OR_ZERO + "%"

# Takes the top of S and puts it in a material line to get rid of it.
# Then pushes a value from %
INPUT_SNIPPET = "%^%<<%>>V%"
# Puts the top of S in the copier. Sends one copy to the shipping
# dock and puts the other back on the stack.
OUTPUT_SNIPPET = "%V%%^%V>%^>>%>>"

# Puts a copy of the top of S into X.
LOOP_TEST_SNIPPET = "%V%%^%V>%V%<^^"

OPEN_LOOP = LOOP_TEST_SNIPPET + "("
CLOSE_LOOP = LOOP_TEST_SNIPPET + ")"

TRANSLATION_DICT = dict(zip("+-<>,.[]", [
    INC_SNIPPET,
    DEC_SNIPPET,
    LEFT_SNIPPET,
    RIGHT_SNIPPET,
    INPUT_SNIPPET,
    OUTPUT_SNIPPET,
    OPEN_LOOP,
    CLOSE_LOOP,
]))

def translation_parts(string):
    """A generator for the translation of each character of the
    brainfuck code."""
    yield START_SNIPPET
    for c in string:
        try:
            yield TRANSLATION_DICT[c]
        except KeyError:
            pass

def translate(string):
    """Convert the brainfuck string to Number Factory code."""
    return "".join(translation_parts(string))

help_msg = """Too many arguments passed (expected 0 or 1).
Usage:
  {0}           - reads STDIN and translates to Number Factory
  {0} <file.bf> - reads file and translates to Number Factory
  {0} -c <code> - translates <code> to Number Factory"""
def usage():
    print(help_msg.format(sys.argv[0]))

def read_file(filename):
    try:
        with open(sys.argv[1]) as f:
            return f.read()
    except IOError:
        print("File could not be read.")
        sys.exit(1)

def main():
    if len(sys.argv) == 1:
        code = sys.stdin.read()
    elif len(sys.argv) == 2:
        code = read_file(sys.argv[1])
    elif len(sys.argv) == 3 and sys.argv[1] == "-c":
        code = sys.argv[2]
    else:
        usage()
        sys.exit()
    print(translate(code))

if __name__ == "__main__":
    main()