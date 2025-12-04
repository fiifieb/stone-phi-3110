IRV: 1@8 2@16 3@5 4@3 5@-10 6@15
# Test immediate shift instructions
slli x10, x1, 2
srli x11, x2, 1
srai x12, x5, 2

# Test more immediate shifts
slli x13, x3, 4
srli x14, x6, 3
srai x15, x5, 1

# Chain shifts
slli x16, x10, 1
srli x17, x11, 2
