IRV: 1@10 2@5 3@0
# Test backward branches (negative offsets)
addi x5, x0, 1
addi x6, x0, 2

# Increment x3 (starts at 0)
addi x3, x3, 1

# Branch backward -1 if x3 < 3 (loops until x3 = 3)
blt  x3, x1, -1

# After loop, x3 = 10 (looped 10 times)
addi x7, x0, 99
