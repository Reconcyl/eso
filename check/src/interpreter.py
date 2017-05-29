import ops_1d, ops_2d

class CheckError(Exception):
    pass

class CheckStack():
    def __init__(self, stack):
        self.stack = stack
    def pop(self):
        if not self.stack:
            raise CheckError("Attempted to pop from empty stack")
        else:
            return self.stack.pop()
    def push(self, obj):
        self.stack.append(obj)
    def clear(self):
        self.stack = []

class Directions():
    linear = 0 # not in 2D mode
    right = 1
    down = 2
    left = 3
    up = 4

def padded_grid(code_string):
    lines = code_string.split("\n")
    max_len = max(len(line) for line in lines)
    # str.ljust() pads with spaces
    return [list(line.ljust(max_len)) for line in lines], max_len

class CheckState():
    @staticmethod
    def get_pair(direction):
        "Turns a direction # (from Directions) into a pair (x, y)."
        return [
            ( 1,  0),
            ( 0,  1),
            (-1,  0),
            ( 0, -1),
        ][direction - 1]
    
    def __init__(self, code, stack, output_func):
        self.stack = CheckStack(stack)
        self.direction = Directions.linear
        self.current_str = None # is not in str
        self.escaped = False # not escaped in a string
        self.register = 0
        self.x = 0
        self.y = 0
        self.grid, self.grid_width = padded_grid(code)
        self.grid_height = len(self.grid)
        self.active = True # whether the program should keep going
        self.output_func = output_func
        
    def move_linear(self):
        self.x += 1
        if self.x == self.grid_width:
            self.x = 0
            self.y += 1
            if self.y == self.grid_height:
                self.active = False
                
    def move_2d(self, direction):
        x_change, y_change = self.get_pair(direction)
        self.x += x_change
        self.y += y_change
        self.x %= self.grid_width
        self.y %= self.grid_height
    
    def move(self):
        if self.direction is Directions.linear:
            self.move_linear()
        else:
            self.move_2d(self.direction)
    
    def tick_string(self):
        if self.escaped:
            self.current_str.append(ord(self.grid[self.y][self.x]))
            self.escaped = False
        else:
            if self.grid[self.y][self.x] == "\"":
                self.stack.push(self.current_str)
                self.current_str = None
            elif self.grid[self.y][self.x] == "\\":
                self.escaped = True
            else:
                self.current_str.append(ord(self.grid[self.y][self.x]))
    
    def tick_lin_op(self):
        command = self.grid[self.y][self.x]
        try:
            operation = ops_1d.all_operations[command]
        except KeyError:
            self.error("Unrecognized command {} in 1D mode".format(command))
        else:
            operation(self)
    
    def tick_linear(self):
        if self.current_str is not None:
            self.tick_string()
        else:
            self.tick_lin_op()
    
    def tick_2d(self):
        command = self.grid[self.y][self.x]
        try:
            operation = ops_2d.all_operations[command]
        except KeyError:
            self.error("Unrecognized command {} in 2D mode".format(command))
        else:
            operation(self)
                
    def tick(self):
        if self.direction is Directions.linear:
            self.tick_linear()
        else:
            self.tick_2d()
        self.move()
    
    @staticmethod
    def error(message):
        raise CheckError(message)
        
def run(code_string, initial_stack, output):
    state = CheckState(code_string, initial_stack, output)
    while state.active:
        state.tick()