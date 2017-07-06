import interpreter

import sys
args = sys.argv[1:]

anything = object()
def match_args(args, pattern):
    if len(args) != len(pattern):
        return False
    for arg, match in zip(args, pattern):
        if match is anything:
            continue
        if isinstance(match, set) and arg not in match:
            return False
        elif arg != match:
            return False
    return True

# poor man's pattern matching for Python
if match_args(args, [anything]):
    code_file_name, input_file, do_tco = args[0], sys.stdin, False
elif match_args(args, ["-i", anything, anything]):
    code_file_name, input_file, do_tco = args[2], open(args[1]), False
elif match_args(args, ["-t", anything]):
    code_file_name, input_file, do_tco = args[1], sys.stdin, True
elif match_args(args, [{"-it", "-ti"}, anything, anything]):
    code_file_name, input_file, do_tco = args[2], open(args[1]), True
else:
    print("Usage:\n"
          "  {0} <code_file>: Runs the code at <code_file>, reading input from STDIN\n"
          "  {0} -i <input_file> <code_file>: Runs the code, reading input from <input_file>\n"
          "  {0} -t <code_file>: Runs the code at <code_file> with tail-call optimization (can be combined with -i)"
          .format(sys.argv[0]), file=sys.stderr)
    sys.exit()

with open(code_file_name) as f:    
    code = f.read()

input_str = input_file.read()
input_file.close()

def puts(obj):
    print(obj, end="")

def error(obj):
    print(obj, file=sys.stderr)
    sys.exit(1)

interpreter.run(code, input_str, puts, error, do_tco)