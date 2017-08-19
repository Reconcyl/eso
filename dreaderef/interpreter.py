import itertools

class InputReader():
    def __init__(self):
        self.buffer = iter("")
    def next_char(self):
        try:
            return next(self.buffer)
        except StopIteration:
            try:
                line = input()
            except EOFError:
                return "\0"
            else:
                self.buffer = iter(itertools.chain(line, "\n"))
                return next(self.buffer)

class Interpreter():
    def __init__(self, nums):
        self.state = {}
        self.state[-1] = 0
        for i, num in enumerate(nums):
            self.state[i] = num
        self.running = True
        self.input_reader = InputReader()
        self.ops = [
            self.end,
            self.deref,
            self.add,
            self.mul,
            self.bool,
            self.numo,
            self.chro,
            self.chri,
        ]
    def get(self, i):
        try:
            return self.state[i]
        except KeyError:
            self.state[i] = 0
            return 0
    def set(self, i, value):
        self.state[i] = value
    def read(self):
        value = self.get(self.state[-1])
        self.state[-1] += 1
        return value
    
    def end(self):
        self.running = False
    def deref(self):
        index = self.read()
        destination = self.read()
        self.set(destination, self.get(index))
    def add(self):
        first = self.read()
        second = self.read()
        destination = self.read()
        self.set(destination, first + second)
    def mul(self):
        first = self.read()
        second = self.read()
        destination = self.read()
        self.set(destination, first * second)
    def bool(self):
        arg = self.read()
        destination = self.read()
        if arg:
            self.set(destination, 1)
        else:
            self.set(destination, 0)
    def numo(self):
        arg = self.read()
        print(arg, end="")
    def chro(self):
        arg = self.read()
        try:
            char = chr(arg)
        except (OverflowError, ValueError):
            pass
        else:
            try:
                print(char, end="")
            except UnicodeEncodeError:
                pass
    def chri(self):
        destination = self.read()
        char = self.input_reader.next_char()
        self.set(destination, ord(char))
    def tick(self):
        op = self.read()
        if 0 <= op < 8:
            self.ops[op]()
        else:
            pass
    def run(self):
        while self.running:
            self.tick()

class LineLexer():
    "Lexes a single line (does not deal with comments)"
    def __init__(self, line, error_callback, line_no):
        self.line = line
        self.pos = 0
        self.error_callback = error_callback
        self.tokens = []
        self.line_no = line_no
        self.running = True
    def add(self, value):
        self.tokens.append(value)
    def next(self):
        char = self.line[self.pos]
        self.pos += 1
        return char
    def back(self):
        self.pos -= 1
    def error(self, msg):
        self.error_callback("Error on line {}: {}"
                                .format(self.line_no, msg))
    def scan_positive(self, char):
        # scans a positive numeric literal
        digits = []
        while char.isdigit():
            digits.append(char)
            char = self.next()
        self.back()
        self.add(["num", int("".join(digits))])
    def scan_negative(self):
        digits = []
        char = self.next()
        if not char.isdigit():
            self.error("Expected digit after -")
        while char.isdigit():
            digits.append(char)
            char = self.next()
        self.back()
        self.add(["num", -int("".join(digits))])
    def scan_name(self, char):
        chars = []
        while char.isalpha():
            chars.append(char)
            char = self.next()
        self.back()
        self.add(["name", "".join(chars)])
    def scan_placeholder(self):
        # Technically, accessing a placeholder value before it is
        # modified is undefined behavior, but this implementation
        # just treats a placeholder as 0.
        self.add(["num", 0])
    def scan_input_token(self):
        self.add(["input"])
    def scan_string(self, quote):
        escaped = False
        chars = []
        char = self.next()
        while escaped or char != quote:
            if escaped:
                escaped = False
                if char == "n":
                    chars.append("\n") # newline escape
                elif char == quote:
                    chars.append(quote) # quote escape
                elif char == "\\":
                    chars.append("\\") # backslash escape
                else:
                    chars.append("\\")
                    chars.append(char)
            else:
                if char == "\\":
                    escaped = True
                else:
                    chars.append(char)
            char = self.next()
            if char == "\n":
                self.error("EOL while scanning string literal")
        self.add(["string", "".join(chars)])
    def scan(self):
        char = self.next()
        if char.isspace():
            pass
        elif char.isalpha():
            self.scan_name(char)
        elif char.isdigit():
            self.scan_positive(char)
        elif char == "-":
            self.scan_negative()
        elif char.isalpha():
            self.scan_name(char)
        elif char == "?":
            self.scan_placeholder()
        elif char == "*":
            self.scan_input_token()
        elif char in "'\"":
            self.scan_string(char)
        if char == "\n":
            self.running = False
    def lex(self):
        while self.running:
            self.scan()
        return self.tokens

class Preprocessor():
    OP_VALUES = {
        "end": 0,
        "deref": 1,
        "add": 2,
        "mul": 3,
        "bool": 4,
        "numo": 5,
        "chro": 6,
        "chri": 7,
    }
    def __init__(self, error_callback, text, args):
        self.nums = []
        self.error_callback = error_callback
        self.text = text
        self.args = args
        self.arg_count = len(args)
        self.arg_index = 0
    def add(self, num):
        self.nums.append(num)
    def strip_comments(self, line):
        # Comments can be put before a line with ".",
        # or after a line with ";".
        line, _, _ = line.partition(";")
        _, _, line = line.rpartition(".")
        return line
    def process(self):
        lines = self.text.split("\n")
        for i, line in enumerate(lines):
            stripped = self.strip_comments(line) + "\n"
            lexer = LineLexer(stripped, self.error_callback, i)
            tokens = lexer.lex()
            for token in tokens:
                if token[0] == "num":
                    self.add(token[1])
                elif token[0] == "name":
                    name = token[1]
                    try:
                        num = self.OP_VALUES[name]
                    except KeyError:
                        self.error_callback("Unknown name {}"
                                                .format(name))
                    else:
                        self.add(num)
                elif token[0] == "string":
                    for char in token[1]:
                        self.add(ord(char))
                elif token[0] == "input":
                    if self.arg_index < self.arg_count:
                        num = self.args[self.arg_index]
                        self.add(num)
                        self.arg_index += 1
                    else:
                        self.error_callback(
                            "Too many inputs requested; "
                            "only {} provided"
                                .format(self.arg_count)
                        )
        return self.nums

if __name__ == "__main__":
    import sys
    def error(msg):
        print(msg, file=sys.stderr)
        sys.exit(1)
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
        try:
            with open(file_name) as f:
                code = f.read()
        except OSError:
            error("Failed to read file")
        args = [int(i) for i in sys.argv[2:]]
        nums = Preprocessor(error, code, args).process()
        Interpreter(nums).run()
    else:
        print("Usage:")
        print("    {} filename [args]".format(sys.argv[0]))
