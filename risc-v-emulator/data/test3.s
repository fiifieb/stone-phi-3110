IRV: 1@10 2@20 3@7 4@1 8@5 9@3 10@12
# Basic arithmetic
add  x11, x1, x2
sub  x12, x2, x1

# Addi immediate tests
addi x13, x1, 100
addi x14, x2, -15

# Shifts
sll x15, x8, x9
srl x16, x8, x9
sra x17, x8, x9

# Move and chain
mv  x18, x10
add x19, x18, x3
sub x20, x19, x4

# Silence x0 writes
add x0, x1, x2
addi x0, x1, 99
mv  x0, x2

# Chained arithmetic to test correctness
add  x21, x11, x12
sll  x22, x21, x4
addi x23, x22, -3