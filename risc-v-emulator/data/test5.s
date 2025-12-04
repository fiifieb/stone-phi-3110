IRV: 1@10 2@20 3@15 4@15 5@5 6@25
# Test logical operations (register-register)
and  x10, x1, x2
or   x11, x1, x2
xor  x12, x1, x3

# More logical ops
and  x13, x3, x4
or   x14, x5, x6
xor  x15, x2, x3

# Test logical operations with immediates
andi x16, x1, 7
ori  x17, x2, 3
xori x18, x3, 12

# Chain logical ops
and  x19, x10, x11
ori  x20, x19, 255
xori x21, x20, 128
