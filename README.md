# 42 Project - Ft_Turing

## Project Overview

This project implements a Turing Machine simulator using Haskell language, that allows users to define, execute, and visualize Turing Machine computations. A Turing Machine is a fundamental concept in computer science that demonstrates the theoretical limits of computation.

### Features

- Define custom Turing Machine configurations in a json file
- Execute step-by-step computations
- Visualize tape operations and state transitions

### Installation Steps

1. Clone the repository:
   ```bash
   git clone "REPOSITORY" ft_turing
   cd ft_turing
   ```

2. Install dependencies:
   ```bash
   cd ressources
   ./install_tools.sh
   ```

Tools installed:
- ghcup 0.1.50.2
- ghc   9.6.7
- cabal 3.12.1.0

## Compilation and Execution

### Compiling the Project

```bash
cabal build
```

### Running the Project

```bash
cabal run ftTuring -- "machine.json" "input"
```

or manually
```bash
./dist-newstyle/build/x86_64-linux/ghc-9.6.7/ftTuring-0.1.0.0/x/ftTuring/build/ftTuring/ftTuring --help
./dist-newstyle/build/x86_64-linux/ghc-9.6.7/ftTuring-0.1.0.0/x/ftTuring/build/ftTuring/ftTuring "machine.json" "input"
```

### Running Test

```bash
cabal test
```

## What is a Turing Machine?

A **Turing Machine** is a mathematical model of computation introduced by Alan Turing in 1936. It serves as a theoretical foundation for understanding what can and cannot be computed.

### Components

A Turing Machine consists of:

1. **Tape**: An infinite strip divided into cells, each containing a symbol from a finite alphabet
2. **Head**: A read/write mechanism that can move left or right along the tape
3. **State Register**: Stores the current state of the machine from a finite set of states
4. **Transition Function**: Defines the machine's behavior based on the current state and symbol

### How It Works

The machine operates in discrete steps:

1. Read the symbol at the current tape position
2. Based on the current state and symbol, the transition function determines:
   - A symbol to write at the current position
   - A direction to move the head (left or right)
   - The next state to transition to
3. Repeat until reaching a halt state

## Authors

- **V0lk7 - jduval** - [Developer] - [GitHub](https://github.com/V0lk7)
- **Rreyth** - [Developer] - [GitHub](https://github.com/Rreyth)

## License

Unlicense
