from enum import auto, Enum
from string import ascii_letters

DEBUG = False

INSTRUCTIONS = [
    "INBOX",
    "OUTBOX",
    "COPYFROM",
    "COPYTO",
    "ADD",
    "SUB",
    "BUMPUP",
    "BUMPDN",
    "JUMP",
    "JUMPZ",
    "JUMPN",
    ]


class TokenType(Enum):
    COMMENT = auto()
    INSTRUCTION = auto()
    JUMP_MARKER = auto()
    GRID_DEFINITION = auto()


class Token():
    def __init__(self, tok_type, line_no):
        self.tok_type = tok_type
        self.line_no = line_no
        self.text = ""

    @staticmethod
    def comment(text, line_no):
        tok = Token(TokenType.COMMENT, line_no)
        tok.text = text
        return tok

    @staticmethod
    def instruction(text, line_no, pointer=False):
        if text not in INSTRUCTIONS:
            raise NameError(f"Unkown instruction: {text}; in line {line_no}")
        tok = Token(TokenType.INSTRUCTION, line_no)
        tok.text = text
        tok.operand = None
        tok.pointer = pointer
        return tok

    @staticmethod
    def jump_marker(text, line_no):
        tok = Token(TokenType.JUMP_MARKER, line_no)
        tok.text = text
        return tok

    @staticmethod
    def grid_definition(size):
        tok = Token(TokenType.GRID_DEFINITION, 1)
        tok.size = size
        return tok

    def __str__(self):
        base = f"<{self.tok_type}>::{self.line_no} -> {self.text}"
        if self.tok_type == TokenType.INSTRUCTION:
            base = f"{base} ({self.operand})"
        return base

    __repr__ = __str__


class Tokenizer():
    def __init__(self, text):
        self.text = iter(text)
        self.line_no = 1
        self.current_char = None

    def advance(self):
        if self.current_char is not None:
            char = self.current_char
            self.current_char = None
            return char
        else:
            try:
                return next(self.text)
            except StopIteration as e:
                return None

    def __iter__(self):
        return self

    def __next__(self):
        return self.next_token()

    def next_token(self):
        char = self.advance()
        if char is None:
            raise StopIteration()
        if char == "~":
            return self.grid_definition()
        if char == " ":
            self.skip_whitespace()
            return self.instruction()
        if char == "-":
            return self.comment()
        if char == "\n":
            self.line_no += 1
            return self.next_token()
        if char in ascii_letters:
            self.current_char = char
            return self.jump_marker()

    def skip_whitespace(self):
        while True:
            char = self.advance()
            if char == " ":
                continue
            else:
                self.current_char = char
                return

    def comment(self):
        self.advance()
        text = []
        while True:
            char = self.advance()
            if char is None: break
            if char == "-":
                char = self.advance()
                if char == "-":
                    break
                else:
                    self.current_char = char
                    continue
            else:
                text.append(char)
        text = "".join(text)
        tok = Token.comment(text, self.line_no)
        return tok

    def instruction(self):
        instruction = []
        while True:
            char = self.advance()
            if char is None: break
            if char in (" ", "\n"):
                break
            else:
                instruction.append(char)
        instruction = "".join(instruction)
        tok = Token.instruction(instruction, self.line_no)
        if char == "\n":
            return tok
        self.skip_whitespace()
        if self.current_char == "\n":
            return tok
        else:
            identifier = []
            while True:
                char = self.advance()
                if char is None: break
                if char == "\n":
                    self.line_no += 1
                    break
                else:
                    identifier.append(char)
            identifier = "".join(identifier)
            if "[" in identifier:
                identifier = identifier.lstrip("[").rstrip("]")
                tok.pointer = True
            tok.operand = identifier
            return tok

    def jump_marker(self):
        name = []
        while True:
            char = self.advance()
            if char is None: break
            if char == ":":
                break
            else:
                name.append(char)
        name = "".join(name)
        tok = Token.jump_marker(name, self.line_no)
        return tok

    def grid_definition(self):
        size = []
        while True:
            char = self.advance()
            if char is None: break
            if char == "~":
                break
            else:
                size.append(char)
        size = int("".join(size))
        tok = Token.grid_definition(size)
        return tok


class SymbolTable():
    def __init__(self):
        self._global_scope = {}
        self.ramend = None # grid size of ingame grid 
        self._instruction_generator = None
        self._jump_marker_generator = None

    @property
    def _instruction_identifier(self):
        if self._instruction_generator is None:
            self._instruction_generator = iter(self._generate_instruction_generator())
        return self._instruction_generator

    @property
    def _jump_marker_identifier(self):
        if self._jump_marker_generator is None:
            self._jump_marker_generator = iter(self._generate_jump_marker_generator())
        return self._jump_marker_generator

    def _generate_instruction_generator(self):
        if self.ramend is None:
            raise ValueError("RAMEND of Symbol Table not defined")
        for i in reversed(range(self.ramend)):
            yield str(i)

    def _generate_jump_marker_generator(self):
        for letter in ascii_letters:
            yield letter

    def instruction_operand(self, index):
        if index not in self._global_scope.keys():
            self._global_scope[index] = next(self._instruction_identifier)
        return self._global_scope[index]

    def jump_marker(self, index):
        if index not in self._global_scope.keys():
            self._global_scope[index] = next(self._jump_marker_identifier)
        return self._global_scope[index]


class ByteCodeAssembler():
    """
    Attributes:
        tokenizer: tokenizer that the assembler feeds from
        code: code that's generated
        instructions: holds all instructions as string mapped to their bytecode - 1 byte length
        jump_marker_table: maps string keys like "a" to their corresponding code adress like 5
        to_patch: maps code adresses to jump_markers; e.g. at adress 5 you should patch label "a" 
    """
    def __init__(self, tokenizer):
        self.tokenizer = tokenizer
        self.code = bytearray()
        self.instructions = {key: self.int_to_1_byte(i) for (i, key) in enumerate(INSTRUCTIONS)}
        self.jump_marker_table = {}
        self.to_patch = {}

    @property
    def program_counter(self):
        """Currently last index into self.code"""
        pc = len(self.code) - 1
        return pc if pc > 0 else 0

    @staticmethod
    def int_to_1_byte(n):
        return n.to_bytes(1, "big")

    @staticmethod
    def int_to_2_bytes(n):
        return n.to_bytes(2, "big")

    def instruction(self, tok):
        self.code.extend(self.instructions[tok.text])
        if "JUMP" in tok.text:
            if tok.operand in self.jump_marker_table.keys():
                jump_adress = self.jump_marker_table[tok.operand]
                self.code.extend(self.int_to_1_byte(jump_adress))
            else:
                self.to_patch[self.program_counter] = tok.operand
                self.code.append(255) # placeholder
        else:
            if tok.operand is not None:
                self.code.extend(self.int_to_2_bytes(int(tok.operand)))

    def jump_marker(self, tok):
        if tok.text in self.jump_marker_table.keys():
            raise SyntaxError(f"Jump marker {tok.text} used multiple times.")
        self.jump_marker_table[tok.text] = self.program_counter
        if tok.text in self.to_patch.values():
            to_patch = self.to_patch.copy()
            pc = self.program_counter
            for key, value in filter(lambda item: tok.text == item[1], self.to_patch.items()):
                self.code[key + 1] = pc
                del to_patch[key]
            self.to_patch = to_patch
            
    def assemble(self):
        for tok in self.tokenizer:
            print(bytes(self.code))
            if tok.tok_type == TokenType.INSTRUCTION:
                self.instruction(tok)
            elif tok.tok_type == TokenType.JUMP_MARKER:
                self.jump_marker(tok)
            else: pass


class ByteCodeDisassembler():
    def __init__(self, bytecode):
        self.bytecode = bytecode
        self.ip = 0
        self.processed_instructions = 0
        self.output = {}
        self.jump_marker_table = {}
        self._marker_names = iter(ascii_letters)

    def two_byte_operand(self):
        return int.from_bytes(self.bytecode[self.ip:self.ip+2], "big")
    
    def one_byte_operand(self):
        return self.bytecode[self.ip]

    def disassemble(self):
        while self.ip < len(self.bytecode) - 1:
            ip = self.ip
            instruction = self.bytecode[self.ip]
            if instruction == 10:
                print("löl")
            self.ip += 1
            try:
                ins = INSTRUCTIONS[instruction]
                self.processed_instructions += 1
            except IndexError as e:
                print(f"Unknown instruction: {instruction} at {ip}")
                raise e
            operand = ""
            if ins in ["COPYFROM", "COPYTO", "ADD", "SUB", "BUMPUP", "BUMPDN"]:
                operand = self.two_byte_operand()
                self.ip += 2
            elif "JUMP" in ins:
                operand = self.one_byte_operand()
                self.ip += 1
                if operand not in self.jump_marker_table.keys():
                    self.jump_marker_table[operand] = next(self._marker_names)
                operand = self.jump_marker_table[operand]
            self.output[ip] = f"    {ins: <9}{operand}"
        for adress, marker in self.jump_marker_table.items():
            text = self.output[adress]
            marked = f"{marker}:\n{text}"
            self.output[adress] = marked

    @property
    def disassemly(self):
        self.disassemble()
        segments = [(key, value) for key, value in self.output.items()]
        segments.sort(lambda item: item[0])
        return "\n".join(segments)


if __name__ == "__main__":
    with open("simple_test.hrm", "r") as f:
        string = f.read()

    # First translation stage
    tokenizer = Tokenizer(string)
    symbol_table = SymbolTable()
    output_buffer = ["-- HUMAN RESOURCE MACHINE PROGRAM --", ""]
    program_counter = 0
    for tok in tokenizer:
        if tok.tok_type == TokenType.GRID_DEFINITION:
            symbol_table.ramend = tok.size
            continue

        if tok.tok_type == TokenType.COMMENT:
            output_buffer.append(f"-- {tok.text} --")
            continue

        if tok.tok_type == TokenType.INSTRUCTION:
            if DEBUG:
                program_counter += 1
                output_buffer.append(f"-- {program_counter:02} --")
            if tok.operand is None:
                operand = ""
            else:
                if "JUMP" in tok.text:
                    operand = symbol_table.jump_marker(tok.operand)
                else:
                    operand = symbol_table.instruction_operand(tok.operand)
            output = f"    {tok.text: <9}"
            if tok.pointer:
                output = f"{output}[{operand}]"
            else:
                output = f"{output}{operand}"
            output_buffer.append(output)
            continue

        if tok.tok_type == TokenType.JUMP_MARKER:
            if DEBUG:
                output_buffer.append(f"")
                output_buffer.append(f"-- {tok.text} --")
            name = symbol_table.jump_marker(tok.text)
            output_buffer.append(f"{name}:")
            continue

    output_buffer.extend(["", "", ""])
    output = "\n".join(output_buffer)

    with open("out.hrm", "w") as f:
        f.write(output)

    # Second translation stage
    asm = ByteCodeAssembler(Tokenizer(output))
    asm.assemble()
    bytecode = bytes(asm.code)
    print(bytecode)

    # Disassembly
    disasm = ByteCodeDisassembler(bytecode)
    print(disasm.disassemly)

    with open("out.hrmc", "wb") as f:
        code = f.write(bytecode)