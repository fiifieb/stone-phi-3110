## 1. Building the Project

From the project root directory:

```
dune build
```

## 2. Running the Emulator

To run the emulator on an input file:

```
dune exec bin/main.exe -- path/to/program.s
```

Make sure your input file follows the expected syntax (IRV declarations followed by RISC‑V instructions).

## 3. Running Tests

### OUnit tests
```
dune test
```
