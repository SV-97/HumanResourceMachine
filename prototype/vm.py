from fancy_translator import INSTRUCTIONS

class Reader():
    """Generator of ints that reads stdin on every next"""
    def __iter__(self):
        return self

    def __next__(self):
        next_value = input("INBOX: ")
        if next_value.isdigit():
            return int(next_value)
        else:
            if len(next_value) > 1:
                print("You may only enter one character at a time")
                return next(self)
            return ord(next_value)


class VM():
    """
    Attributes:
        code (bytearray): code that's to be executed
        accu (int): accumulator that stores ALU results
        ip (int): instruction pointer into code
        source (iterator): iterator that produces values for inbox calls
        memory (dict): mapping ingame "tile indices" to their values
    """
    def __init__(self, code, source):
        self.code = code
        self.accu = None
        self.ip = 0
        # move to dict.fromkeys(...) with python 3.7 to have constant lookup time
        # prior to 3.7 dict isn't sorted (and I can't be bothered to use OrderedDict)
        self.instruction_mapping = [
            self.inbox, # 0
            self.outbox, # 1
            self.copyfrom, # 2
            self.copyto, # 3
            self.add, # 4
            self.sub, # 5
            self.bumpup, # 6
            self.bumpdn, # 7
            self.jump, # 8
            self.jumpz, # 9
            self.jumpn, # 10
            ]
        self.source = source
        self.memory = {}

    def two_byte_operand(self):
        return int.from_bytes(self.code[self.ip:self.ip+2], "big")
    
    def one_byte_operand(self):
        return self.code[self.ip]

    def inbox(self):
        self.accu = next(self.source)

    def outbox(self):
        print("OUTBOX: ", self.accu)
        self.accu = None

    def copyfrom(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.accu = self.memory[operand]

    def copyto(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.memory[operand] = self.accu

    def add(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.accu += self.memory[operand]

    def sub(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.accu -= self.memory[operand]

    def bumpup(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.memory[operand] += 1

    def bumpdn(self):
        operand = self.two_byte_operand()
        self.ip += 2
        self.memory[operand] -= 1

    def jump(self):
        self.ip = self.one_byte_operand()

    def jumpz(self):
        if self.accu == 0:
            self.ip = self.one_byte_operand()
        else:
            self.ip += 2 # shift over target jump adress

    def jumpn(self):
        if self.accu < 0:
            self.ip = self.one_byte_operand()
        else:
            self.ip += 2 # shift over target jump adress

    def cpu(self):
        while True:
            instruction = self.code[self.ip]
            print(f"{instruction} -> {self.instruction_mapping[instruction].__qualname__} -> {INSTRUCTIONS[instruction]}")
            self.ip += 1
            self.instruction_mapping[instruction]() # offset from 0
            if self.ip > len(self.code):
                if self.ip - len(self.code) > 1:
                    print("Referenced memory that was out of bounds")
                break


if __name__ == "__main__":
    with open("out.hrmc", "rb") as f:
        code = f.read()
    
    source = Reader()
    vm = VM(code, source)
    vm.memory[24] = 0
    vm.cpu()