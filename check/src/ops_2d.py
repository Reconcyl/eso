all_operations = {}
def operator(char):
    def decorate(func):
        all_operations[char] = func
    return decorate

@operator("#")
def swap_modes(state):
    state.direction = 0 # Directions.linear

@operator(">")
def left(state):
    state.direction = 1 # Directions.right

@operator("v")
def down(state):
    state.direction = 2 # Directions.down
    
@operator("<")
def right(state):
    state.direction = 3 # Directions.right

@operator("^")
def up(state):
    state.direction = 4 # Directions.up

@operator(" ")
def nop(state):
    pass