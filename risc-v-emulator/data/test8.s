IRV: 1@1000 2@2000 3@100 4@200
# Test memory operations
# Setup some values in registers
addi x10, x0, 42
addi x11, x0, 255
addi x12, x0, 1234

# Store operations - store values to memory
sw x10, 0(x1)
sb x11, 4(x1)
sd x12, 8(x1)

# More stores with offsets
sw x10, 100(x2)
sb x11, 104(x2)
sd x12, 108(x2)

# Load operations - load values from memory
lw x20, 0(x1)
lb x21, 4(x1)
ld x22, 8(x1)

# Load with offsets
lw x23, 100(x2)
lb x24, 104(x2)
ld x25, 108(x2)

# Test with register x3 and x4 as base
sw x10, 50(x3)
lw x26, 50(x3)
sb x11, 75(x4)
lb x27, 75(x4)
