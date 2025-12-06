IRV: 1@100 2@200
# Test JAL - jump and link
jal  x10, 3
addi x11, x0, 1
addi x12, x0, 2
addi x13, x0, 3

# Test JALR - jump and link register
addi x14, x0, 6
jalr x15, 2(x14)
addi x16, x0, 4
addi x17, x0, 5

# Simple JAL forward
jal  x18, 2
addi x19, x0, 6
addi x20, x0, 7
