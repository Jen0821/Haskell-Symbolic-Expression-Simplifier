# 🧮 Haskell Symbolic Arithmetic Expression Simplifier

## Overview

This coursework assignment involves implementing a simplifier for **symbolic arithmetic expressions** in **Haskell**. The system handles expressions involving addition, multiplication, and non-negative integer exponentiation. The core strategy is to convert expressions into a **canonical polynomial form** to achieve unambiguous simplification, eliminating redundant elements, often referred to as 'junk'.

## Data Structures

### `Expr` (Symbolic Expression)

An Algebraic Data Type (ADT) representing the expressions:

* `NumLit`: Integer literals.
* `ExpX`: $x$ raised to a non-negative power.
* `Op BinOp Expr Expr`: Binary operations (addition/multiplication) applied to sub-expressions.

### `Poly` (Polynomial Representation)

The canonical form for polynomials, represented as a **list of coefficients** ordered by descending power. For example, the polynomial $x^3 + 2x - 1$ is represented as the list `[1, 0, 2, -1]`.

## Core Implementation Details

### 1. Junk-Free Arithmetic

Custom implementations of addition (`add`) and multiplication (`mul`) for `Expr` are designed to prevent the introduction of **'junk'** (e.g., $0 + E$ simplifies to $E$, $1 \cdot E$ simplifies to $E$) directly during expression construction.

### 2. Conversion Functions

The project relies on converting between the symbolic `Expr` type and the structured `Poly` type:

* `exprToPoly :: Expr -> Poly`: Converts a general symbolic expression into its canonical polynomial representation. This function is crucial for expression **normalization**.
* `polyToExpr :: Poly -> Expr`: Converts the canonical polynomial form back into a simplified, 'junk-free' symbolic expression.

### 3. Simplification Pipeline

* `simplify :: Expr -> Expr`: The main function that simplifies any given expression by leveraging the polynomial conversion process:
    $$
    \text{simplify}(E) = \text{polyToExpr}(\text{exprToPoly}(E))
    $$

## Compilation and Usage

The project is built using **Cabal**.

```bash
# Build the project
cabal build

# Load the module in GHCi to test expression creation and simplification
cabal repl
