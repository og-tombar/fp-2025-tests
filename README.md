# Functional Programming 2025 Tests

This repository contains test suites for the Functional Programming course homework assignments.

## Important Note

The tests in this repository were only tested on a macOS machine. They might also work on a Windows machine, but this might require further configurations not listed in this document.

## Contributing

- I try to do my best adding test cases that cover all examples in the homework PDF files. If you feel like the coverage is partial and you want to add more test cases, contact me and I will add you as a contributor to this repository.
- It is possible that there might be errors or inconsistencies in some of the tests, so use your judgement and don't take them for granted.
- In both of the scenarios above, contact me at og.tombar@gmail.com.

## Prerequisites

Make sure you have Stack (The Haskell Tool Stack) installed and configured as specified in the first recitation slides, otherwise the tests won't work.

## Using the Tests

Clone this repository to your local machine:

```bash
git clone https://github.com/og-tombar/fp-2025-tests
cd fp-2025-tests
```

### Prepare Your Solution

Place your homework solution file in the corresponding `src` directory:

- For HW2: Copy your `HW2.hs` to `hw2/src/`
- For HW3: Copy your `HW3.hs` to `hw3/src/`
- And so on for other assignments

### Run the Tests

1. Change to the homework directory you want to test. For example:

   ```bash
   cd hw2    # for testing HW2
   # or
   cd hw3    # for testing HW3
   ```

2. Run the test suite:
   ```bash
   stack test
   ```

If this is the first time you use Stack to run tests, this might take a minute or two as it might run updates or install missing dependencies.

## Additional Information

- Each homework directory contains its own set of tests in the `test` directory
- If you encounter any build issues, try running `stack clean` followed by `stack test`

## Current Directory Structure

```
hw1/
  ├── src/        # Place HW1.hs here
  └── test/       # Test suite for HW1

hw2/
  ├── src/        # Place HW2.hs here
  └── test/       # Test suite for HW2

hw3/
  ├── src/        # Place HW3.hs here
  └── test/       # Test suite for HW3

hw4/
  ├── src/        # Place HW4.hs here
  └── test/       # Test suite for HW4
```
