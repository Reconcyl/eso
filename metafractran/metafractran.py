import sys
import re
import pathlib
import fractions as fr

path = pathlib.Path(sys.argv[1]).resolve()
if not path.is_file():
    print("File does not exist")
    sys.exit(1)

name, *parts = reversed(path.parts)
is_fraction = re.compile(r"^(\d+):(\d+)$")
fractions = []
for i in parts:
    match = is_fraction.match(i)
    if not match:
        break
    num, denom = match.groups()
    fractions.append(fr.Fraction(int(num), int(denom)))
fractions.reverse() # they were added backwards

is_valid_name = re.compile(r"(\d+)(?:\..*)?")
match = is_valid_name.match(name)
if not match:
    print("File name is not valid")
    sys.exit(1)
    
num = int(match.group(1))

# run the fractran program

def run_fractran(num, fractions):
    while True:
        for i in fractions:
            product = num * i
            if product.denominator == 1:
                num = int(product)
                break
        else:
            return num

print(run_fractran(num, fractions))