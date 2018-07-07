import itertools

import builtin
import parse

class FoamStack():
    def __init__(self, state):
        self.state = state
        self.stack = []
    def push(self, item):
        self.stack.append(item)
    def pop(self, expect=object):
        if not self.stack:
            self.state.error("Popped from empty stack")
        item = self.stack.pop()
        if not isinstance(item, expect):
            self.state.error("Expected type {}, got {}".format(expect.__name__, type(item).__name__))
        return item
    def pop_n(self, n):
        results = []
        for _ in range(n):
            results.append(self.stack.pop())
        return results[::-1]
    def push_many(self, items):
        self.stack.extend(items)
    def pop_many(self, n):
        result = []
        for _ in range(n):
            result.append(self.pop())
        return result[::-1]
    def get_stack_copy(self):
        return self.stack[:]

class InputParser():
    DIGITS = "1234567890"
    def __init__(self, state, input_string):
        self.state = state
        self.string_len = len(input_string)
        self.string = input_string
        self.position = 0
    def more(self):
        return (self.position < self.string_len)
    def get_char(self, offset=0):
        pos = self.position + offset
        if pos >= self.string_len:
            self.state.error("Out of input")
        c = self.string[pos] 
        return c
    def advance(self):
        self.position += 1
    def read_char(self):
        c = self.get_char()
        self.advance()
        return c
    def read_int(self):
        while True:
            c = self.get_char()
            if c in self.DIGITS:
                break
            elif c == "-" and self.get_char(offset=1) in self.DIGITS:
                break
            self.advance()
        digits = [c]
        self.advance()
        while True:
            if not self.more():
                break
            c = self.get_char()
            if c not in self.DIGITS:
                break
            digits.append(c)
            self.advance()
        return int("".join(digits))
    def read_rest(self):
        string = self.string[self.position:]
        self.position = len(self.string)
        return string

class FoamState():
    def __init__(self, input_str, output_func, error_func, do_tco):
        
        self.output = output_func
        self.error = error_func
        
        self.stack = FoamStack(self)
        self.input_parser = InputParser(self, input_str)
        
        self.code = []
        self.do_tco = do_tco
        
    def add_frame(self, frame):
        self.code.append(frame[::-1])
    def remove_frame(self):
        self.code.pop()
    def get_frame(self, i):
        try:
            return self.code[~i]
        except IndexError:
            self.error("Tried to access illegal frame")
    def extend_frame(self, i, extension):
        self.get_frame(i).extend(extension[::-1])
    def push_to_frame(self, i, obj):
        self.get_frame(i).append(obj)
    def pop_from_frame(self, i):
        frame = self.get_frame(i)
        if not frame:
            self.error("Tried to pop from empty frame")
        return self.get_frame(i).pop()
    def get_frame_copy(self, i):
        return self.get_frame(i)[::-1]
    
    def execute_op(self, name):
        if name in builtin.bootstrapped:
            self.add_frame(builtin.bootstrapped[name])
        elif name in builtin.core_ops:
            builtin.core_ops[name](self)
        else:
            self.error("Error: unknown operation {}".format(repr(name)))

    def tick(self):
        obj = self.pop_from_frame(0)
        if self.do_tco and not self.get_frame(0):
            # Tail call optimization.
            # This can optionally be used, but it is off by default as it may break self-modifying code.
            self.remove_frame()
        if isinstance(obj, str):
            self.execute_op(obj)
        else:
            self.stack.push(obj)
        # remove all empty frames
        while self.code and not self.get_frame(0):
            self.remove_frame()
    
    def depth(self):
        return len(self.code)
    
    def run(self, code):
        "Keep tick()-ing until the code's depth descends below what it was at first."
        self.add_frame(code)
        stack_depth = self.depth()
        while self.depth() >= stack_depth:
            self.tick()
    
def run(code, input_str, output_func, error_func, do_tco):
    state = FoamState(input_str, output_func, error_func, do_tco)
    parsed = parse.parse(state, code)
    state.run(parsed)