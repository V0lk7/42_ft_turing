# 0ⁿ1ⁿ Language Turing Machine

A Turing machine implementation that determines whether a string belongs to the language L = {0ⁿ1ⁿ | n ≥ 0}.

## Overview

This Turing machine checks if an input string has the same number of 0's followed by the same number of 1's. It verifies strings of the form 0ⁿ1ⁿ, where all 0's must come before all 1's, and outputs:
- **Y** (Yes) if the string matches the pattern 0ⁿ1ⁿ
- **N** (No) if the string does not match

## Machine Specification

- **Input alphabet**: `[0, 1]`
- **Blank Symbol**: `[.]`
- **Alphabet**: `[0, 1, Y, N] + [.]`
- **Initial State**: `q0`
- **Final State**: `HALT`
- **States**: `q0, q1, q2, q3, q4, HALT`

## How It Works

The machine uses a matching strategy to verify equal counts:

1. **Mark First 0**: Starting from the leftmost position, mark a '0' by replacing it with blank (.)
2. **Traverse to End**: Move right through all remaining 0's and 1's to reach the rightmost '1'
3. **Mark Last 1**: Replace the rightmost '1' with blank (.)
4. **Return Left**: Move back to the beginning
5. **Repeat**: Continue matching pairs of 0's and 1's until:
   - All symbols are matched → Output 'Y'
   - Mismatch detected (wrong order, unequal counts) → Output 'N'

### State Descriptions

- **q0**: Initial state - reads the leftmost unprocessed symbol or checks if done
  - If '0': mark it and start matching process
  - If '.': empty or all matched → accept with 'Y'
  - If '1': wrong format (1's before 0's) → reject
- **q1**: Traversal state - moves right through the string to find the end
- **q2**: End verification - checks the rightmost symbol
  - If '1': mark it, return left to continue matching
  - If '.': no 1's left but 0's remain → reject with 'N'
- **q3**: Return state - moves left back to the beginning to process next pair
- **q4**: Rejection state - moves to end of tape and writes 'N'
- **HALT**: Final halting state

## State Transition Diagram
<p align="center">
  <img src="diagram.png">
</p>

## Transition Table

| Current State | Next State  | Read Symbol | Write Symbol | Move Direction |
|:-------------:|:-----------:|:-----------:|:------------:|:--------------:|
| q0 | q1 | 0 | . | RIGHT  |
| q0 | HALT | . | Y | RIGHT  |
| q0 | q4 | 1 | 1 | RIGHT  |
| q1 | q1 | 0 | 0 | RIGHT  |
| q1 | q1 | 1 | 1 | RIGHT  |
| q1 | q2 | . | . | LEFT   |
| q2 | q3 | 1 | . | LEFT   |
| q2 | q4 | . | . | RIGHT  |
| q2 | q4 | 1 | 1 | RIGHT  |
| q3 | q3 | 1 | 1 | LEFT   |
| q3 | q3 | 0 | 0 | LEFT   |
| q3 | q0 | . | . | RIGHT  |
| q4 | q4 | 1 | 1 | RIGHT  |
| q4 | q4 | 0 | 0 | RIGHT  |
| q4 | HALT | . | N | RIGHT  |

## Example Executions

WAITING PROGRAM TO RUN MACHINE AND PUT EXEMPLE IN THE MARKDOWN

## Usage

To run this Turing machine:

1. Load the `0n1n.json` configuration file
2. Provide an input string composed of '0' and '1' characters
3. The machine will process the string and halt with either 'Y' or 'N' on the tape

## Implementation Notes

- The machine uses the blank symbol (`.`) to mark processed positions
- Empty strings are accepted (n=0 is valid)
- The string must have all 0's before all 1's (no interleaving)
- The counts of 0's and 1's must be exactly equal
