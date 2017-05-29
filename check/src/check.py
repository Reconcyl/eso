import sys, ast

import interpreter

def malformed_input():
    print("Error: malformed input", file=sys.stderr)
    sys.exit(2)

def checkify(obj):
    if isinstance(obj, int):
        return obj
    elif isinstance(obj, list):
        return [checkify(i) for i in obj]
    else:
        malformed_input()

with open(sys.argv[1]) as f:
    code = f.read()

def puts(string):
    print(string, end="")

inputs = [ast.literal_eval(i) for i in sys.argv[2:]]
try:
    interpreter.run(code, inputs, puts)
except interpreter.CheckError as e:
    print("\nError: {}".format(e))
    sys.exit(3)