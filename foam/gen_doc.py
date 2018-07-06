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

# Remove a prefix from a string. Throw an error
# if the string does no have that prefix.
def remove_prefix(prefix, string):
    if string.startswith(prefix):
        return string[len(prefix):]
    else:
        raise ValueError(prefix)

def wrap(string, wrapper):
    return wrapper + string + wrapper

# Turn a string into an acceptable Markdown code snippet
# that displays as that string. For example, "abc" gets
# turned into "`abc`", and "`" gets turned into "`` ` ``".
def markdown_code(string):
    if not string:
        return ""
    if string.startswith("`"):
        string = " " + string
    if string.endswith("`"):
        string = " " + string
    max_backticks = longest_run(string, "`")
    return wrap(string, "`" * (max_backticks + 1))

# Make a string into acceptable Markdown text by escaping
# special characters within it.
MARKDOWN_SPECIAL_CHARACTERS = "`*_|"
def markdown_escape(string):
    def chars():
        for char in string:
            if char in MARKDOWN_SPECIAL_CHARACTERS:
                yield "\\"
            yield char
    return "".join(chars())

# Make a string into acceptable Markdown bold.
def markdown_bold(string):
    return wrap(markdown_escape(string), "**")

# Make a formatted, aligned, and escaped Markdown table
# with bold headers and code data.
def markdown_table(headers, rows_data):
    def make_column(header, column_data):
        column = [markdown_bold(header), *map(markdown_code, column_data)]
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
    args, result = usage_raw.split(" -> ")
    return " ".join(["Usage:",
                    markdown_code(args),
                    "→",
                    markdown_code(result)])

def generate_builtin_description_markdown(name, description, usage, examples):
    return "\n\n".join([
        "## " + markdown_code(name),
        description,
        generate_usage_message(usage),
        "### Examples",
        markdown_table(["Code", "Result"], examples)
    ])

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
    try:
        while True:
            name = lines.next()
            description = []
            while True:
                try:
                    line = remove_prefix("| ", lines.next())
                except ValueError:
                    lines.back()
                    break
                description.append(line)
            if not description:
                raise SyntaxError("Description must start with '| '.")
            description = "\n".join(description)
            usage = lines.next()
            examples = []
            while True:
                try:
                    code = remove_prefix("  ", lines.next())
                    result = remove_prefix("> ", lines.next())
                except ValueError:
                    lines.back()
                    break
                examples.append((code, result))
            if not examples:
                raise SyntaxError("Examples must be given.")
            yield BuiltinEntry(name, description, usage, examples)
            while lines.next() == "":
                pass
            lines.back()
    except IndexError:
        pass

def main():
    with open("builtins_raw.txt") as f:
        content = f.read()
    entries = list(get_entries(content.split("\n")))
    entries.sort(key=lambda entry: entry.name)
    with open("builtins.md", "w") as f:
        f.write("# Builtins")
        for entry in entries:
            f.write("\n\n")
            f.write(entry.to_markdown())
    print("Finished.\n")

if __name__ == "__main__":
    main()