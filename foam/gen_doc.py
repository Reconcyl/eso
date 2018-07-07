# Return the length of the longest run of a given element
# within the iterable.
def longest_run(iterable, element):
    longest = 0
    current = 0
    for i in iterable:
        if i == element:
            current += 1
            if current > longest:
                longest = current
        else:
            current = 0
    return longest

class ParseError(Exception):
    pass

# Remove a prefix from a string. Throw an error
# if the string does no have that prefix.
def remove_prefix(prefix, string):
    if string.startswith(prefix):
        return string[len(prefix):]
    else:
        raise ValueError(prefix)

def wrap(string, wrapper):
    return wrapper + string + wrapper

# Put a backslash before the characters specified in `special`.
def escape_special(special, string):
    def chars():
        for char in string:
            if char in special:
                yield "\\"
            yield char
    return "".join(chars())

# Escape `|` characters in a string.
def markdown_bar_escape(string):
    return escape_special("|", string)

# Turn a string into an acceptable Markdown code snippet
# that displays as that string. For example, "abc" gets
# turned into "`abc`", and "`" gets turned into "`` ` ``".
def markdown_code(string, table=False):
    if not string:
        return ""
    if "\n" in string:
        return "<pre>" + markdown_escape(string).replace("\n", "<br/>") + "</pre>"
    if string.startswith("`"):
        string = " " + string
    if string.endswith("`"):
        string = string + " "
    if table:
        # Code snippets can only use the `|` escape inside tables. 
        string = markdown_bar_escape(string)
    
    max_backticks = longest_run(string, "`")
    return wrap(string, "`" * (max_backticks + 1))

# Make a string into acceptable Markdown text by escaping
# special characters within it.
def markdown_escape(string):
    return escape_special("`*_|&<>", string)

# Make a string into acceptable Markdown bold.
def markdown_bold(string):
    return wrap(markdown_escape(string), "**")

# Make a formatted, aligned, and escaped Markdown table
# with bold headers and code data.
def markdown_table(headers, rows_data):
    def make_column(header, column_data):
        column = [markdown_bold(header),
                  *(markdown_code(datum, table=True)
                   for datum in column_data)]
        max_width = max(map(len, column))
        column = [elem.ljust(max_width) for elem in column]
        column.insert(1, "-" * max_width)
        column = [wrap(elem, " ") for elem in column]
        return column
    def format_row(row):
        return wrap("|".join(row), "|")
    columns_data = zip(*rows_data)
    columns_text = (make_column(header, data)
                    for header, data in zip(headers, columns_data))
    rows_text = (format_row(row) for row in zip(*columns_text))
    return "\n".join(rows_text)

def generate_usage_message(usage_raw):
    args, _, result = usage_raw.partition(" -> ")
    return " ".join(["Usage:",
                    markdown_code(args),
                    "→",
                    markdown_code(result)])

class ListTraversal():
    def __init__(self, target):
        self.target = target
        self.index = 0
    def next(self):
        elem = self.target[self.index]
        self.index += 1
        return elem
    def back(self):
        self.index -= 1

class BuiltinEntry():
    def __init__(self, name, description, usage, examples):
        self.name = name
        self.description = description
        self.usage = usage
        self.examples = examples
    def to_markdown(self):
        return "\n\n".join([
            "## " + markdown_code(self.name),
            self.description,
            generate_usage_message(self.usage),
            "### Examples",
            markdown_table(["Code", "Result"], self.examples)
        ])

def get_entries(lines):
    lines = ListTraversal(lines)
    def lines_with_prefix(prefix, error):
        result = []
        while True:
            try:
                line = remove_prefix(prefix, lines.next())
            except ValueError:
                lines.back()
                break
            result.append(line)
        if not result:
            raise error()
        return "\n".join(result)
    try:
        while True:
            name = lines.next()
            description = lines_with_prefix(
                "| ",
                lambda: ParseError(lines.index,
                                   "Expected description block prefixed with '| '"))
            usage = lines.next()
            examples = []
            while True:
                try:
                    code = lines_with_prefix("  ", ValueError)
                    result = lines_with_prefix("> ", ValueError)
                except ValueError:
                    break
                examples.append((code, result))
            if not examples:
                raise ParseError(lines.index, "Examples must be given.")
            yield BuiltinEntry(name, description, usage, examples)
            while lines.next() == "":
                pass
            lines.back()
    except IndexError:
        pass

def main():
    import sys
    with open("builtins_raw.txt") as f:
        content = f.read()
    lines = content.split("\n")
    if lines[-1] != "":
        lines.append("")
    try:
        entries = list(get_entries(lines))
    except ParseError as error:
        index, message = error.args
        print("Parse error at line {}: {}".format(index, message))
        sys.exit(1)
    entries.sort(key=lambda entry: entry.name)
    with open("builtins.md", "w") as f:
        f.write("# Foam Builtins\n\n")
        f.write("This file lists all {} builtins".format(len(entries)))
        f.write(" that are currently available in Foam, "
                "as well as what they do.")
        for entry in entries:
            f.write("\n\n")
            f.write(entry.to_markdown())
    print("Finished.\n")

if __name__ == "__main__":
    main()