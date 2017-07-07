import ast
import sys
import os

# This code was stolen from the 05AB1E unit test code, which can be found here:
# https://github.com/Adriandmen/05AB1E/blob/master/test/unit_tests.py#L1-L6
current_dir = os.path.dirname(os.path.abspath(__file__))
parent_dir = os.path.dirname(current_dir)
sys.path.insert(0, parent_dir)

import interpreter
import parse

def print_indent(obj):
    print("    ", obj, sep="")

def group_by_2(lines):
    lines = list(lines)
    return list(zip(lines[0::2], lines[1::2]))

def parse_stack(string):
    obj = ast.literal_eval(string)
    if not isinstance(obj, tuple):
        obj = (obj,)
    return list(obj)

passed = True
class SandboxedRun():
    class Terminate(Exception):
        pass
    def output(self, _):
        pass
    def error(self, msg):
        self.error_msg = msg
        raise self.Terminate
    def __init__(self, code):
        self.code = code
        self.error_msg = ""
        state = interpreter.FoamState("", self.output, self.error, False)
        try:
            state.run(parse.parse(state, code))
        except self.Terminate:
            pass
        self.result_stack = state.stack.get_stack_copy()
    def assert_stack(self, stack):
        global passed
        if self.error_msg:
            passed = False
            print("The test")
            print_indent(self.code)
            print("errored with the message")
            print_indent(self.error_msg)
            print("when it should have generated a result stack of:")
            print_indent(stack)
        elif self.result_stack != stack:
            passed = False
            print("The test")
            print_indent(self.code)
            print("generated a result stack of")
            print_indent(self.result_stack)
            print("when it should have generated a result stack of:")
            print_indent(stack)
    def assert_error(self, msg):
        global passed
        if not self.error_msg:
            passed = False
            print("The test")
            print_indent(self.code)
            print("did not error and generated a result stack of")
            print_indent(self.result)
            print("when it should have errored with the message:")
            print_indent(self.msg)
        elif self.error_msg != msg:
            passed = False
            print("The test")
            print_indent(self.code)
            print("errored with the message")
            print_indent(self.error_msg)
            print("when it should have errored with the message:")
            print_indent(msg)

def strip_comments(lines):
    for line_ in lines:
        line = line_.rstrip("\n")
        if line and not line.startswith("n"):
            yield line

def run_tests(lines):
    for code, expect in group_by_2(strip_comments(lines)):
        result = SandboxedRun(code)
        if expect.startswith("-> "):
            stack = parse_stack(expect[3:])
            result.assert_stack(stack)
        elif expect.startswith("! "):
            msg = expect[2:]
            result.assert_error(msg)

with open("code_tests") as f:
    run_tests(f)
    if passed:
        print("All tests passed.")