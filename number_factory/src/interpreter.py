import sys

from collections import deque, defaultdict

# These functions can be modified by the module user if necessary.
def input_fn():
    return input()
def output_fn(s):
    print(s, end="")

def read_input_integer():
    """Keep trying to read a line parsable as an integer from STDIN
    until we get one (in which case, return it) or we hit EOF (in
    which case, return -1)."""
    while True:
        try:
            s = int(input_fn())
        except (EOFError, StopIteration):
            return -1
        except ValueError:
            pass
        else:
            return s

def alpha_chr(c):
    if 0 <= c <= 27:
        return " ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"[c]
    else:
        return ""

class Room:
    """A room which holds a value and has no special behavior when
    the robot enters, picks something up, or puts something down."""
    def __init__(self):
        self.content = None
    def put(self, c):
        self.content = c
    def get(self):
        old_content = self.content
        self.content = None
        return old_content

class MaterialLine(Room):
    """A room which consumes anything placed in it by the robot and
    which always provides a constant value."""
    def __init__(self, c):
        self.content = c
    def put(self, _):
        pass
    def get(self):
        return self.content

class BinaryRoom(Room):
    """A base class for any room which waits for two numbers to be
    placed before executing an action."""
    def execute(self, placed):
        raise RuntimeError
    def put(self, c):
        if self.content is None:
            self.content = c
        else:
            self.execute(c)

class FusingMachine(BinaryRoom):
    """A binary room which adds the second value to the first."""
    def execute(self, new):
        self.content += new

class CarvingMachine(BinaryRoom):
    """A binary room which subtracts the second value from the
    first."""
    def execute(self, new):
        self.content -= new

class TargetRoom(Room):
    """A base class for a room which can place things in another
    room."""
    def __init__(self, target):
        self.target = target
        self.content = None
    def send(self, c):
        self.target.put(c)

class Copier(TargetRoom):
    """A room which copies everything that is placed in it to its
    target."""
    def put(self, c):
        self.content = c
        self.send(c)

class DequeRoom(Room):
    """A base class for a room containing a deque where placing
    something puts it at the front of the deque, and taking something
    puts it at either the front or the back."""
    def __init__(self):
        raise RuntimeError
    def put(self, c):
        self.content.append(c)
    def get(self):
        try:
            return self.pop()
        except IndexError:
            return None
    def pop(self):
        raise RuntimeError

class StackRoom(DequeRoom):
    """A deque room using a stack."""
    def __init__(self):
        self.content = []
    def pop(self):
        return self.content.pop()

class QueueRoom(DequeRoom):
    """A deque room using a queue."""
    def __init__(self):
        self.content = deque()
    def pop(self):
        return self.content.popleft()

class ShippingDock(Room):
    """A room where attempting to place a number sends it to STDOUT.
    """
    def put(self, c):
        output_fn(str(c) + "\n")

class Disposal(Room):
    """A room that is always empty and ignores any attempt to put
    something in it."""
    def put(self, _):
        pass

class QualityControl(BinaryRoom, TargetRoom):
    """A binary target room where putting in a second number sends 1
    or 0 depending on whether the two numbers were equal."""
    def execute(self, new):
        self.send(int(self.content == new))
        self.content = None

class UniquenessDetector(BinaryRoom, TargetRoom):
    """A binary target room where putting in a second number sends 1
    or 0 depending on whether the two numbers were different."""
    def execute(self, new):
        self.send(int(self.content != new))
        self.content = None

class InvertingTransformer(Room):
    """A room where a 0 is converted into a 1 upon being placed and
    all other numbers are converted to 0."""
    def put(self, c):
        if c == 0:
            self.content = 1
        else:
            self.content = 0

class CommandCenter(Room):
    """A room that starts with value 1 and serves as the initial
    location for the robot as well as a target for conditionals."""
    def __init__(self):
        self.content = 1

class PostalOffice(Room):
    """A room where placing a number causes a letter, newline, or
    space to be sent to STDOUT."""
    def put(self, c):
        output_fn(alpha_chr(c))

class Factory:
    """The state of each room of the factory."""
    ROBOT_INITIAL_POSITION = (2, 3)
    RECEIVING_DOCK_POSITION = (4, 0)
    def __init__(self):
        # The receiving dock is the only dock with special behavior
        # upon entry. Rather than adding a separate "entry" method,
        # this room is special-cased by the main interpreter loop.
        self.receiving_dock = Room()
        self.copy_dropoff = Room()
        self.command_center = CommandCenter()

        self.map = [
            [
                MaterialLine(0),
                MaterialLine(1),
                MaterialLine(2),
                MaterialLine(10),
                self.receiving_dock,
            ], [
                CarvingMachine(),
                StackRoom(),
                StackRoom(),
                QueueRoom(),
                ShippingDock(),
            ], [
                FusingMachine(),
                Copier(self.copy_dropoff),
                self.copy_dropoff,
                Room(), # The storage closet.
                Disposal(),
            ], [
                QualityControl(self.command_center),
                UniquenessDetector(self.command_center),
                self.command_center,
                InvertingTransformer(),
                PostalOffice(),
            ]
        ]

class State:
    """The state of the program, including the position of the robot,
    what it is holding (if anything), and the factory."""
    def __init__(self):
        (self.x, self.y) = Factory.ROBOT_INITIAL_POSITION
        self.factory = Factory()
        self.held = None
    def check_for_receiving_dock(self):
        # The receiving dock has special behavior upon entry that is
        # not shared with any other cell, so it is special-cased by
        # the interpreter.
        if (self.x, self.y) == Factory.RECEIVING_DOCK_POSITION:
            number = read_input_integer()
            self.current_room().put(number)
    def change_x(self, dx):
        self.x += dx
        self.x %= 5
        self.check_for_receiving_dock()
    def change_y(self, dy):
        self.y += dy
        self.y %= 4
        self.check_for_receiving_dock()
    def current_room(self):
        return self.factory.map[self.y][self.x]
    def pick_up(self):
        self.held = self.current_room().get()
    def put_down(self):
        self.current_room().put(self.held)
        self.held = None
    def run_commands(self, commands):
        for command in commands:
            command.run_with(self)

class CommandUp:
    """A command directing the robot to go up."""
    def run_with(self, state):
        state.change_y(-1)

class CommandDown:
    """A command directing the robot to go down."""
    def run_with(self, state):
        state.change_y(1)

class CommandLeft:
    """A command directing the robot to go left."""
    def run_with(self, state):
        state.change_x(-1)

class CommandRight:
    """A command directing the robot to go right."""
    def run_with(self, state):
        state.change_x(1)

class CommandPick:
    """A command that tries to put down whatever the robot is holding
    or tries to pick something up if the robot isn't holding
    anything."""
    def run_with(self, state):
        if state.held is None:
            state.pick_up()
        else:
            state.put_down()

class CommandLoop:
    """A command that repeatedly runs the commands in its body until
    the number in the command center is 0."""
    def __init__(self, body):
        self.body = body
    def run_with(self, state):
        while state.factory.command_center.content != 0:
            state.run_commands(self.body)

class ParseError(Exception):
    """Represents an error encountered while parsing."""
    pass

command_translations = defaultdict(lambda: "", {
    '^': "CommandUp(),",
    'V': "CommandDown(),",
    '<': "CommandLeft(),",
    '>': "CommandRight(),",
    '%': "CommandPick(),",
    '(': "CommandLoop([",
    ')': "]),"
})
def parse(source):
    # Just convert the source into a Python expression and try to
    # evaluate it, because I'm too lazy to write a real parser.
    python_expr = "".join(
        map(command_translations.__getitem__, source))
    try:
        return eval('[' + python_expr + ']')
    except SyntaxError:
        raise ParseError

def run_from_string(source, state):
    state.run_commands(parse(source))

def run_from_file(filename):
    with open(filename) as f:
        source = f.read()
    run_from_string(source, State())

help_msg = """Number Factory (NF) interpreter: too many arguments passed
(expected 0 or 1)
Usage:
  {0} <file.nf> - runs the file
  {0}           - starts an interactive console"""
def usage():
    print(help_msg.format(sys.argv[0]),
          file=sys.stderr)
    sys.exit(1)

def console():
    state = State()
    while True:
        line = input()
        if line == "exit":
            print("Bye.")
            return
        try:
            run_from_string(line, State)
        except ParseError:
            print("No parse", file=sys.stderr)

def main():
    if len(sys.argv) == 1:
        console()
    if len(sys.argv) == 2:
        try:
            run_from_file(sys.argv[1])
        except ParseError:
            print("No parse", file=sys.stderr)
    else:
        usage()

if __name__ == "__main__":
    main()