class Builder():
    def __init__(self):
        self.result = []
        self.deep = [self.result]
    def add(self, obj):
        self.deep[-1].append(obj)
    def open(self):
        opened = []
        self.add(opened)
        self.deep.append(opened)
    def close(self):
        self.deep.pop()
    def depth(self):
        return len(self.deep)

class Parser():
    WHITESPACE = " \n\t"
    def __init__(self, state, code):
        self.code = code
        self.code_len = len(code)
        self.state = state
        self.parsed = Builder()
        self.i = 0
        
    def more(self):
        return (self.i < self.code_len)
    def current(self):
        return self.code[self.i]
    def normal_char(self, char):
        if char in self.WHITESPACE:
            return False
        elif char in "[]|":
            return False
        else:
            return True
    def advance(self):
        self.i += 1
        
    def handle_current(self):
        char = self.current()
        if char in self.WHITESPACE:
            self.advance()
        elif char == "[":
            self.parsed.open()
            self.advance()
        elif char == "]":
            if self.parsed.depth() > 1:
                self.parsed.close()
            else:
                self.state.error("Parse error: unmatched ]")
            self.advance()
        elif char == "|":
            self.parsed.add(self.read_barred())
        else:
            self.parsed.add(self.read_unbarred())
            
    def read_unbarred(self):
        token = []
        while True:
            if not self.more():
                break # we hit EOF while scanning, so just end the token here
            char = self.current()
            if not self.normal_char(char):
                break
            token.append(char)
            self.advance()
        return "".join(token)
    
    def read_barred(self):
        self.advance() # move past the initial bar
        token = []
        escaped = False
        while True:
            if not self.more():
                self.state.error("Parse error: unfinished barred token")
            char = self.current()
            if char == "|" and not escaped:
                break
            elif char == "\\" and not escaped:
                escaped = True
            else:
                token.append(char)
                escaped = False
            self.advance()
        self.advance() # move past the final bar
        return "".join(token)
        
    def parse(self):
        while self.more():
            self.handle_current()
        if self.parsed.depth() != 1:
            self.state.error("Parse error: unmatched [")
        return self.parsed.result

def parse(state, code):
    return Parser(state, code).parse()