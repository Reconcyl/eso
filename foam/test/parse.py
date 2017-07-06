import sys
import os

current_dir = os.path.dirname(os.path.realpath(__file__))
parent_dir = os.path.dirname(current_dir)
sys.path.insert(0, parent_dir)

class Terminate(Exception):
    pass

import parse
class FakeState():
    def __init__(self):
        self.error_msg = ""
    def error(self, msg):
        self.error_msg = msg
        raise Terminate()

passed = True
START_AT = 0

def error(string):
    global passed
    print(string)
    passed = False

def assert_result(num, code, expected):
    if num < START_AT:
        return
    state = FakeState()
    result = parse.parse(state, code)
    if state.error_msg:
        error("Test {} failed: {} should be {}, but errors as {}"
                  .format(num, repr(code), expected, state.error_msg))
    elif result != expected:
        error("Test {} failed: {} should be {}, but got {}"
                  .format(num, repr(code), expected, result))

def assert_errors(num, code):
    if num < START_AT:
        return
    state = FakeState()
    try:
        result = parse.parse(state, code)
    except Terminate:
        pass
    else:
        error("Test {} failed: {} should error".format(num, repr(code),))

# START_AT = 11 # threshold (skip previous tests, only use when creating new tests)

assert_result(1, "123", ["123"])
assert_result(2, "123 456", ["123", "456"])
assert_result(3, "123 \n\t 456", ["123", "456"])
assert_result(4, "   1   ", ["1"])
assert_result(5, "", [])
assert_result(6, "||", [""])
assert_result(7, "|abc||def|", ["abc", "def"])
assert_result(8,  "abc |def| ghi", ["abc", "def", "ghi"])
assert_result(9,  "abc|def|ghi", ["abc", "def", "ghi"])
assert_result(10, "|special \n[characters]|", ["special \n[characters]"])
assert_errors(11, "|...")
assert_result(12, r"|\escapes|", ["escapes"])
assert_result(13, r"|esc\|ape|", ["esc|ape"])
assert_result(14, r"|esc\\ape|", [r"esc\ape"])
assert_errors(15, r"|the only bars are \|escaped\|")
assert_errors(16, "|\\")
assert_result(17, ";#[0 1]} -*r[:*.]_*", [";#", ["0", "1"], "}", "-*r", [":*."], "_*"])
assert_result(18, ".'Hello, World!", [".'Hello,", "World!"])
assert_result(19, ".'|Hello, World!|", [".'", "Hello, World!"])
assert_result(20, "[[[[[[|[[[[]]]]|]]]]]]", [[[[[[["[[[[]]]]"]]]]]]])
assert_errors(21, "[")
assert_errors(22, "]")
assert_errors(23, "]abc[")
assert_result(24, r"||\||", ["", "\\", ""])

if passed:
    print("All tests passed.")