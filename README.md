# sparsevectors

<!-- badges: start -->
[![R-CMD-check](https://github.com/jasonlu2/sparsevectors/workflows/R-CMD-check/badge.svg)](https://github.com/jasonlu2/sparsevectors/actions)
<!-- badges: end -->

An R package providing an efficient sparse numeric vector class with arithmetic operations, statistical methods, and standardization capabilities.

## Overview

The `sparsevectors` package implements a memory-efficient sparse numeric vector class for R. Instead of storing all elements of a vector (including zeros), the sparse representation only stores non-zero values and their positions. This approach is particularly useful for large vectors with many zero elements, enabling significant memory savings and faster computations.

## Features

- **Sparse Numeric Class**: Efficient storage of sparse vectors
- **Arithmetic Operations**: Addition, subtraction, and element-wise multiplication of sparse vectors
- **Statistical Functions**: Mean, Euclidean norm, and standardization
- **Utility Functions**: Sparsity calculation and cross product (dot product)
- **Coercion Methods**: Easy conversion between dense and sparse representations
- **Visualization**: Plotting capabilities for comparing sparse vectors

## Installation

You can install the development version of sparsevectors from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jasonlu/sparsevectors")
```

## Usage

### Creating Sparse Vectors

```r
library(sparsevectors)

# Create a sparse vector from a dense vector
dense_vec <- c(0, 1, 0, 2, 0, 3, 0, 0, 0)
sparse_vec <- as(dense_vec, "sparse_numeric")
print(sparse_vec)
```

### Arithmetic Operations

```r
# Create two sparse vectors
x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
y <- as(c(0, 4, 0, 5, 0), "sparse_numeric")

# Addition
z <- x + y
print(z)

# Element-wise multiplication
w <- x * y
print(w)
```

### Statistical Operations

```r
# Calculate mean (accounts for all zeros)
mean_val <- mean(sparse_vec)
print(mean_val)

# Calculate Euclidean norm
norm_val <- norm(sparse_vec)
print(norm_val)

# Standardize the vector
standardized <- standardize(sparse_vec)
print(standardized)
```

### Utility Functions

```r
# Calculate sparsity percentage
sparsity_pct <- sparsity(sparse_vec)
print(sparsity_pct)

# Compute dot product
dot_product <- sparse_crossprod(x, y)
print(dot_product)
```

## Performance Benefits

The sparse representation provides significant advantages for vectors with many zeros:

- **Memory Efficiency**: Only non-zero elements are stored
- **Computational Speed**: Operations only process non-zero elements
- **Scalability**: Suitable for very large vectors

For example, a vector of length 1,000,000 with only 100 non-zero elements requires minimal storage compared to the dense equivalent.

## Documentation

For detailed documentation of all functions, visit the [package website](https://jasonlu.github.io/sparsevectors/) or use:

```r
?sparse_numeric
?sparse_add
?mean.sparse_numeric
?norm
?standardize
```

## Development

This package was developed as part of the SDS 375 Data Product Development class.

## License

MIT License - see the [LICENSE](LICENSE) file for details.
