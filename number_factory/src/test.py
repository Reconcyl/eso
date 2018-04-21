import traceback, sys

import interpreter

def execute(source, input_str):
    """Replace the input and output functions on the """
    output = []
    interpreter.input_fn = iter(input_str.split("\n")).__next__
    interpreter.output_fn = output.append
    interpreter.run_from_string(source, interpreter.State())
    return "".join(output)

def test(source, expected="", input_string=""):
    try:
        actual = execute(source, input_string)
    except Exception as e:
        print("Test failed: an exception was raised.")
        print("Code:", source)
        traceback.print_exception(None, e, sys.exc_info()[2], file=sys.stdout)
        sys.exit(1)
    if actual != expected:
        print("Test failed: unexpected output.",
              "The expected output was {},".format(repr(expected)),
              "But actual output was {}.".format(repr(actual)),
              sep="\n")
        print("Code:", source)
        sys.exit(1)

# Rooms not tested:
#   S, since it is identical to P
#   C, since it has the same mechanics as X
#   /, since it has the same mechanics as =

# The empty string parses normally.
test("")
# Invalid commands don't trigger a syntax error.
test("invalid commands")

# Room 0 has value 0.
test("V<<%V<%", expected="0\n")
# Room 1 has value 1.
test("V<%V<<%", expected="1\n")
# Room 2 has value 2.
test("V%V>>%", expected="2\n")
# Room T has value 10.
test("V>%V>%", expected="10\n")
# Material lines can be used an arbitrary number of times.
test("V %V>>%<<^ %V>>%<<^ %V>>%<<^", expected="2\n2\n2\n")
# Placing a value into a material line does nothing.
test("V %>% %V>%", expected="10\n")

# Room X has the value expected.
test("%^^>>%", expected="1\n")
# Room X is not infinite like the material lines.
test("%^^>>% <<^^ %^^>>%", expected="1\n")

# The fusing machine can be used to add numbers.
test("""%^<<%  -  put 1 on +
        <<^^%  -  take 10 from T
        VV>>%  -  put 10 on +
        %<^%   -  take and output""",
    expected="11\n")
# You can take something out of an operator machine.
test("""%^<<%  -  put 1 on +
        %%%    -  take, put, take
        ^^%VV  -  get rid of the number
        %<^%   -  take and output""")
# The carving machine can be used to subtract numbers.
test("""V>%   -  take 10 from T
        V>>%  -  put 10 on -
        ^>%   -  take 1 from 1
        V<%   -  put 1 on -
        %<%   -  take and output""",
    expected="9\n")
# You can subtract and get a negative number.
test("""V%V<<%  -  put 2 in -
        <<^%    -  take 10 from T
        V>>%    -  put 10 in -
        %<%     -  take and output""",
    expected="-8\n")

# The copier and copy dropoff are initially empty.
test("""^<%<<^%   -  take from # and output
        V<<%>>^%  -  take from @ and output""")
# The copier puts anything placed in it into the dropoff.
test("""%^<%   -  put 1 in #
        %<<^%  -  take and output
        V<<    -  go to @
        %>>^%  -  take and output""",
    expected="1\n1\n")
# Placing something in the copier overwrites anything in the dropoff.
test("""%^%     -  put 1 in @
        ^^%     -  take 2 from 2
        <VV%    -  put 2 in #
        >%>>^%  -  take from @ and output""",
    expected="2\n")

# Room P works as a stack.
test("""V%V%    -  put 2 on P
        ^<%>V%  -  put 1 on P
        %>>%<<  -  take and output
        %>>%<<  -  take and output""",
    expected="1\n2\n")
# Room P is empty if the stack is empty.
test("VV%>>%")

# Room Q has functionality expected.
test("""V%>V%  -  put 2 on Q
        ^%V%   -  put 10 on Q
        %>%<   -  take and output
        %>%<   -  take and output""",
    expected="2\n10\n")
# Room Q is empty if the stack is empty.
test("VV>%>%")

# The receiving dock can be used to read input.
test("V>>%V%", expected="-4\n", input_string="-4")
# The receiving dock reads whenever the robot enters it.
test("V>><>%V%", expected="5\n", input_string="4\n5\n")
# The receiving dock ignores any line that doesn't parse as a decimal integer.
test("V>>%V%", expected="8\n", input_string="hello\n3.1415\n0x5f3759df\n8\n9")
# Without any input, the receiving dock defaults to -1.
test("V>>%V%^%V%", expected="3\n-1\n", input_string="3")

# The disposal deletes any item put into it.
test("""%^>>%  -  put 1 in &
        %^%    -  take and output""")

# The equality control checks the equality of the numbers placed in it.
test("""%<<%   -  put 1 in =
        V%^%   -  put 0 in =
        >>%    -  take from X
        >>^^%  -  output""",
    expected="0\n")
# The equality control is empty after being passed its numbers.
test("""<<V      -  go to 0
        %^%V%^%  -  put 0 in = twice
        %<^^%    -  take from = and output
        VV<<%    -  take from X
        >>^^%    -  output""",
    expected="1\n")

# Putting a 0 in the inverting transformer results in a 1.
test("""V<<%   -  take 0 from 0
        ^<<%   -  put 1 in !
        %>^^%  -  take and outut""",
    expected="1\n")
# Putting any other number on the transformer results in a 0.
test(""">V%^%  -  put 10 on !
        %>^^%  -  take and output""",
    expected="0\n")

# Putting a 0 on the postal office prints a space.
test("<<V%^<%", expected=" ")
# Putting a number 1-26 on the postal office prints the corresponding letter.
test("%>>%", expected="A")
test(">V%^>%", expected="J")
# Putting 27 on the postal office prints a newline.
test(""">V          -  go to T
        %VV>>%<<^^  -  put 10 in +
        %VV>>%<<^^  -  put 10 in +
        %VV>>%      -  put 10 in +
        %^%         -  take and put in -
        ^>>%<<V%    -  put 2 in -
        ^>%<V%      -  put 1 in -
        %<VV%       -  take and put in ~""",
    expected="\n")
# Putting any other number in the postal office doesn't print anything.
test("""<<V%V%  -  put 0 in -
        ^>%<V%  -  put 1 in -
        %<VV%   -  take and put in ~""")
test("""V>%VV<<%  -  put 10 in #
        >%<<%>    -  take from @ and put in +
        %%        -  take, put
        >%<<%>    -  take from @ and put in +
        %<%       -  take and put in +
        %<V%      -  take and put in ~""")

print("All tests executed successfully.")