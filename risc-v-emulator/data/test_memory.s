IRV: 1@1000 2@42
# Simple memory test - store and load
# x1 = base address (1000)
# x2 = value to store (42)

# Store x2 to memory at address x1 + 0
sw x2, 0(x1)

# Load from memory at address x1 + 0 into x3
lw x3, 0(x1)

# x3 should now equal 42
