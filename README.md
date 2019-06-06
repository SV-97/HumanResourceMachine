# HumanResourceMachine
A bit of a higher level assembly for HRM as well as a compiler to HRM code, a bytecode compiler and a bytecode VM

## Bytecode encoding
Instructions are encoded as one byte  
Jump destination operands are one byte as well  
Other operands are encoded as two bytes - a 1 in the highest bit signals that it's to be interpreted as a pointer