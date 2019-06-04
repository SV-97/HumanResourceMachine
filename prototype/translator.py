import re
from string import ascii_letters

ascii_letters = iter(ascii_letters)

declarations = [
    "zero", 
    "index", 
    "sorted", 
    "left", 
    "right", 
    "one",
    "str_length"
    ]

instructions = [
    "INBOX",
    "OUTBOX",
    "COPYFROM",
    "COPYTO",
    "ADD",
    "SUB",
    "BUMPUP",
    "BUMPDN",
    "JUMPZ",
    "JUMPN",
    "JUMP", # important that jump is behind jumpz etc.
    ]

declarations = {key: 24-i for (i, key) in enumerate(declarations)}

with open("bubble_sort.hrm", "r") as f:
    string = f.read()

"""Get jump markers and replace with letters"""
for line in string.split("\n"):
    jump_marker = re.match(r"(.+):", line)
    if jump_marker is not None:
        jump_marker = jump_marker.group(1)
        if jump_marker not in declarations.keys():
            declarations[jump_marker] = next(ascii_letters)

# string = re.sub("--.*--", "", string) # remove comments
while "\n\n" in string:
    string = string.replace("\n\n", "\n")
for key, value in declarations.items():
    pattern = f" {key}"
    string = re.sub(pattern, f" {value}", string)
    pattern = f"\[{key}\]"
    string = re.sub(pattern, f"[{value}]", string)
    pattern = f"(?m)^{key}:"
    string = re.sub(pattern, f"\n-- {key} --\n{value}:", string)

for instruction in instructions:
    if "JUMP" in instruction:
        pattern = f" +{instruction} +"
    else:
        pattern = f" +{instruction} *"
    string = re.sub(pattern, f"    {instruction: <9}", string)

with open("out.hrm", "w") as f:
    f.write(f"-- HUMAN RESOURCE MACHINE PROGRAM --\n{string}\n\n\n")