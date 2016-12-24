# an interpreter for the Betaload esolang
# usage: interpreter.py <filename> or interpreter.py -c for the console

import sys, copy

class Stack:
    def __init__(self, stackType):
        self.type = stackType
        self.stack = []
    def push(self, value):
        if not isinstance(value, self.type):
            raise TypeError("value must be of type " + self.type.__name__)
        self.stack.append(value)
    def error(self):
        raise RuntimeError("stack too small for operation")
    def pop(self):
        if len(self.stack) == 0:
            self.error()
        return self.stack.pop()
    def swap(self):
        temp1 = self.pop()
        temp2 = self.pop()
        self.push(temp1)
        self.push(temp2)
    def __getitem__(self, item):
        if len(self.stack) <= item:
            self.error()
        return self.stack[~item] # wow, never thought I'd use that operator...
    def dup(self, doCopy=False):
        top = self[0]
        if doCopy:
            top = copy.deepcopy(top)
        self.push(top)

class BetaloadEnv:
    def __init__(self):
        self.strStack = Stack(str)
        self.envStack = Stack(BetaloadEnv) # nope, no recursion here
        self.active = self.strStack # the active stack
    def switch(self): # toggle active stacks
        self.active = self.envStack \
            if self.active is self.strStack \
            else self.strStack
    def betaloadRepr(self): # return the code to create the environment
        if len(self.strStack.stack) == len(self.envStack.stack) == 0:
            return "#"
        else:
            return "#{" \
                + "".join("(" + s + ")" for s in self.strStack.stack) \
                + "".join(s.betaloadRepr() for s in self.envStack.stack) \
                + "}"
        

rootEnv = BetaloadEnv()
environments = Stack(BetaloadEnv) # the path to the current environment
initEnv = copy.copy(environments) # a save that can be returned to on error
env = rootEnv # the current active environment
didOutput = False # whether there was any output
i = 0
code = ""
def interpret(programCode):
    global environments, env, didOutput, i, code
    didOutput = False
    length = len(code)
    code = programCode
    i = 0
    while i < length:
        if code[i] == "~": # swap
            env.active.swap()
        elif code[i] == ":": # dup
            env.active.dup(doCopy=True)
        elif code[i] == "!": # drop
            env.active.pop()
        elif code[i] == "*": # concat
            a = env.strStack.pop()
            b = env.strStack.pop()
            env.strStack.push(b + a)
        elif code[i] == "(": # string literal
            nest = 1
            string = ""
            while nest > 0: # handle matching parentesis; (()) will push () to the stack
                string += code[i]
                i += 1
                if code[i] == "(":
                    nest += 1
                elif code[i] == ")":
                    nest -= 1
            env.strStack.push(string[1:])
        elif code[i] == "a": # like repr
            a = env.active.pop()
            if env.active is env.strStack: # on the string stack
                env.strStack.push("(" + a + ")")
            else: # on the env stack
                env.strStack.push(env.envStack.pop().betaloadRepr()) # hurray for OO!
        elif code[i] == "^": # exec, but without recursion
            code = code[:i+1] + env.strStack.pop() + code[i+1:]
            length = len(code)
        elif code[i] == "S": # print
            print(env.strStack.pop(), end="")
            didOutput = True
        elif code[i] == "R": # input
            chrMapping = {}
            while len(env.strStack[0]) == 1:
                char = env.strStack.pop()
                code = env.strStack.pop()
                chrMapping[char] = code
            env.strStack.pop() # the last value is not a single character, so it is deleted
            inputString = input()
            newString = ""
            for c in inputString:
                if c in chrMapping:
                    newString += chrMapping[c]
            env.strStack.push(newString)
            didOutput = True # at least, text was written to the console (what you typed)
        elif code[i] == ";":
            env.switch()
        elif code[i] == "#":
            env.envStack.push(BetaloadEnv())
        elif code[i] == ">":
            a = env.active.pop()
            if env.active is env.strStack:
                env.envStack[0].strStack.push(a)
            else:
                env.envStack[0].envStack.push(a)
        elif code[i] == "<":
            if env.active is env.strStack:
                env.strStack.push(env.envStack[0].strStack.pop())
            else:
                env.envStack.push(env.envStack[0].envStack.pop())
        elif code[i] == "{":
            newEnv = env.envStack[0]
            environments.push(env)
            env = newEnv # env gets set after, but the above line must be executed first for error handling
        elif code[i] == "}":
            env = environments.pop()
        elif code[i] in [" ", "\t", "\n"]:
            pass
        else:
            raise RuntimeError("unrecognized character " + code[i])
        i += 1 

def error():
    global i, code
    print("Error: " + sys.exc_info()[1].args[0])
    print(code)
    print(" " * i + "^")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage:",
              "{0} <filename> : executes the file".format(sys.argv[0]),
              "{0} -c : interactive console".format(sys.argv[0]),
              sep="\n")
        sys.exit(1) # bad user input

    if sys.argv[1] == "-c":
        while True:
            if didOutput:
                print() # newline
            code = input("b > ")
            if code == "quit":
                break
            try:
                interpret(code) # the state is saved
                if not didOutput:
                    print(rootEnv.betaloadRepr()) # print the contents of the environment if the program had no output itself
            except RuntimeError:
                error()
                # Return to the root environment on error, so things like #{!}
                # won't leave you in the environment it created
                environments = copy.copy(initEnv)
                env = rootEnv
    else:
        try:
            with open(sys.argv[1], "r") as f:
                code = f.read()
                try:
                    interpret(code)
                except RuntimeError:
                    error()
                    sys.exit(2) # Betaload program errored
        except IOError:
            print("Couldn't read the source file.")
            sys.exit(1)
