IRV: 1@10 2@10 3@5 4@20
# Test branch instructions
addi x5, x0, 100

# BEQ - branch if equal (should branch, offset +2)
beq  x1, x2, 2
addi x6, x0, 1
addi x7, x0, 2

# BNE - branch if not equal (should branch, offset +2)
bne  x1, x3, 2
addi x8, x0, 3
addi x9, x0, 4

# BLT - branch if less than (should branch, offset +2)
blt  x3, x4, 2
addi x10, x0, 5
addi x11, x0, 6

# BGE - branch if greater or equal (should branch, offset +2)
bge  x2, x3, 2
addi x12, x0, 7
addi x13, x0, 8

# Test non-taken branches
bne  x1, x2, 2
addi x14, x0, 99
addi x15, x0, 88
