library(testthat)
library(sparsevectors)

# Test validity method
test_that("validity method works correctly", {
    # Valid sparse vector
    expect_true({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3, 1),
                 pos = c(1L, 2L, 3L, 5L),
                 length = 5L)
        validObject(x)
    })

    # Invalid: value and pos different lengths
    expect_error({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3),
                 pos = c(1L, 2L, 3L, 5L),
                 length = 5L)
        validObject(x)
    })

    # Invalid: position out of bounds
    expect_error({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3),
                 pos = c(1L, 2L, 8L),
                 length = 5L)
        validObject(x)
    })

    # Invalid: duplicate positions
    expect_error({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3),
                 pos = c(1L, 2L, 2L),
                 length = 5L)
        validObject(x)
    })

    # Invalid: length not single integer
    expect_error({
        x <- new("sparse_numeric",
                 value = c(1, 2, 3),
                 pos = c(1L, 2L, 3L),
                 length = c(5L, 6L))
        validObject(x)
    })
})

# Test coercion methods
test_that("coercion from numeric to sparse works", {
    result <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    expect_s4_class(result, "sparse_numeric")
    expect_equal(result@value, c(1, 2))
    expect_equal(result@pos, c(4L, 5L))
    expect_equal(result@length, 5L)
})

test_that("coercion from sparse to numeric works", {
    sparse_vec <- new("sparse_numeric",
                      value = c(1, 2),
                      pos = c(2L, 4L),
                      length = 5L)
    dense_vec <- as(sparse_vec, "numeric")
    expect_equal(dense_vec, c(0, 1, 0, 2, 0))
})

# Test show method
test_that("show method works", {
    sparse_vec <- new("sparse_numeric",
                      value = c(1, 2),
                      pos = c(2L, 4L),
                      length = 5L)
    expect_output(show(sparse_vec), "Sparse numeric vector of length 5")
    expect_output(show(sparse_vec), "\\[2\\] = 1")
    expect_output(show(sparse_vec), "\\[4\\] = 2")
})

# Test arithmetic operators exist
test_that("arithmetic operators are available", {
    expect_true(existsMethod("+", c("sparse_numeric", "sparse_numeric")))
    expect_true(existsMethod("-", c("sparse_numeric", "sparse_numeric")))
    expect_true(existsMethod("*", c("sparse_numeric", "sparse_numeric")))
})

# Test sparse_add
test_that("sparse_add works correctly", {
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    result <- sparse_add(x, y)

    expect_s4_class(result, "sparse_numeric")
    expected <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
    expect_equal(result, expected)
})

test_that("sparse_add with dense vectors works", {
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    result <- sparse_add(x, y)

    expected <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
    expect_equal(result, expected)
})

test_that("sparse_add fails with different lengths", {
    x <- as(c(1, 2), "sparse_numeric")
    y <- as(c(1, 2, 3), "sparse_numeric")
    expect_error(sparse_add(x, y))
})

# Test operator overloading
test_that("operator overloading works", {
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")

    expect_equal(x + y, sparse_add(x, y))
    expect_equal(x - y, sparse_sub(x, y))
    expect_equal(x * y, sparse_mult(x, y))
})

# Test sparse_crossprod
test_that("sparse_crossprod works", {
    x <- as(c(1, 0, 2, 0), "sparse_numeric")
    y <- as(c(0, 3, 0, 4), "sparse_numeric")
    result <- sparse_crossprod(x, y)
    expect_equal(result, 0)  # No overlapping positions

    x <- as(c(1, 2, 3), "sparse_numeric")
    y <- as(c(4, 5, 6), "sparse_numeric")
    result <- sparse_crossprod(x, y)
    expect_equal(result, 1*4 + 2*5 + 3*6)  # 32
})

# Test sparsity function
test_that("sparsity function works", {
    x <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
    expect_equal(sparsity(x), 40)  # 2 non-zero out of 5

    x <- as(rep(0, 10), "sparse_numeric")
    expect_equal(sparsity(x), 0)

    x <- as(rep(1, 5), "sparse_numeric")
    expect_equal(sparsity(x), 100)
})

# Test mean function
test_that("mean function works", {
    x <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
    expect_equal(mean(x), 0.6)  # (1+2)/5 = 0.6

    x <- as(rep(0, 10), "sparse_numeric")
    expect_equal(mean(x), 0)

    x <- as(c(1, 2, 3, 4, 5), "sparse_numeric")
    expect_equal(mean(x), 3)
})

# Test norm function
test_that("norm function works", {
    x <- as(c(3, 0, 4, 0), "sparse_numeric")
    expect_equal(norm(x), 5)  # sqrt(3^2 + 4^2) = 5

    x <- as(c(1, 2, 2), "sparse_numeric")
    expect_equal(norm(x), sqrt(1 + 4 + 4))  # sqrt(9) = 3

    x <- as(rep(0, 5), "sparse_numeric")
    expect_equal(norm(x), 0)
})

# Test standardize function
test_that("standardize function works", {
    # Simple case: mean=0, sd=1 already
    x <- as(c(0, 0, 0), "sparse_numeric")
    result <- standardize(x)
    expect_equal(result@value, numeric(0))
    expect_equal(result@pos, integer(0))

    # Case with actual values
    x <- as(c(1, 2, 3, 4, 5), "sparse_numeric")
    result <- standardize(x)

    # Check that result has mean approximately 0
    expect_equal(mean(result), 0, tolerance = 1e-10)

    # Check that norm is approximately sqrt(n-1) for standardized vector
    # (since we lose 1 degree of freedom for mean)
    expect_equal(norm(result), sqrt(4), tolerance = 1e-10)
})

# Test plot method exists
test_that("plot method exists", {
    expect_true(existsMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

# Test generics exist
test_that("generics are properly defined", {
    expect_true(isGeneric("sparse_add"))
    expect_true(isGeneric("sparse_mult"))
    expect_true(isGeneric("sparse_sub"))
    expect_true(isGeneric("sparse_crossprod"))
    expect_true(isGeneric("sparsity"))
    expect_true(isGeneric("norm"))
    expect_true(isGeneric("standardize"))
})

# Test formals
test_that("function formals are correct", {
    expect_true(length(formals(sparse_add)) >= 2)
    expect_true(length(formals(sparse_mult)) >= 2)
    expect_true(length(formals(sparse_sub)) >= 2)
    expect_true(length(formals(sparse_crossprod)) >= 2)
})

# Additional edge case tests
test_that("operations with all-zero vectors work", {
    x <- as(rep(0, 5), "sparse_numeric")
    y <- as(rep(0, 5), "sparse_numeric")

    expect_equal(sparse_add(x, y)@value, numeric(0))
    expect_equal(sparse_mult(x, y)@value, numeric(0))
    expect_equal(sparse_sub(x, y)@value, numeric(0))
    expect_equal(sparse_crossprod(x, y), 0)
    expect_equal(mean(x), 0)
    expect_equal(norm(x), 0)
})

test_that("operations with single element vectors work", {
    x <- as(5, "sparse_numeric")
    y <- as(3, "sparse_numeric")

    expect_equal(as(sparse_add(x, y), "numeric"), 8)
    expect_equal(as(sparse_mult(x, y), "numeric"), 15)
    expect_equal(as(sparse_sub(x, y), "numeric"), 2)
    expect_equal(sparse_crossprod(x, y), 15)
    expect_equal(mean(x), 5)
    expect_equal(norm(x), 5)
})

# Test standardization edge cases
test_that("standardize handles constant vectors", {
    x <- as(c(2, 2, 2, 2), "sparse_numeric")
    result <- standardize(x)
    expect_equal(result@value, numeric(0))  # All become zero after standardization
    expect_equal(result@length, 4)
})

# Additional comprehensive tests for 90% coverage

# Test mean function with various inputs
test_that("mean function comprehensive tests", {
    # Test with mixed positive/negative values
    x <- as(c(-1, 0, 2, 0, -3), "sparse_numeric")
    expect_equal(mean(x), (-1 + 2 - 3) / 5)  # -0.4

    # Test with single non-zero value
    x <- as(c(0, 5, 0), "sparse_numeric")
    expect_equal(mean(x), 5/3)

    # Test with all zeros
    x <- as(rep(0, 10), "sparse_numeric")
    expect_equal(mean(x), 0)

    # Test with large numbers
    x <- as(c(1e6, 0, 2e6), "sparse_numeric")
    expect_equal(mean(x), (1e6 + 2e6) / 3)
})

# Test norm function with various inputs
test_that("norm function comprehensive tests", {
    # Test with negative values
    x <- as(c(-3, 0, -4), "sparse_numeric")
    expect_equal(norm(x), 5)  # sqrt(9 + 16) = 5

    # Test with decimals
    x <- as(c(0.5, 0, 0.866), "sparse_numeric")
    expect_equal(norm(x), 1, tolerance = 1e-3)  # Approximately 1

    # Test with single element
    x <- as(7, "sparse_numeric")
    expect_equal(norm(x), 7)

    # Test with large numbers
    x <- as(c(3000, 0, 4000), "sparse_numeric")
    expect_equal(norm(x), 5000)  # sqrt(9e6 + 16e6) = sqrt(25e6) = 5000
})

# Test standardize function comprehensive tests
test_that("standardize function comprehensive tests", {
    # Test with known result
    x <- as(c(1, 2, 3, 4, 5), "sparse_numeric")
    result <- standardize(x)

    # Mean should be 0
    expect_equal(mean(result), 0, tolerance = 1e-15)

    # Standard deviation should be 1
    # For n=5, std = sqrt(sum((x-mean)^2)/(n-1))
    expected_norm <- sqrt(4)  # n-1 = 4
    expect_equal(norm(result), expected_norm, tolerance = 1e-15)

    # Test with negative values
    x <- as(c(-2, -1, 0, 1, 2), "sparse_numeric")
    result <- standardize(x)
    expect_equal(mean(result), 0, tolerance = 1e-15)
    expect_equal(norm(result), sqrt(4), tolerance = 1e-15)

    # Test with decimal values
    x <- as(c(0.1, 0.2, 0.3), "sparse_numeric")
    result <- standardize(x)
    expect_equal(mean(result), 0, tolerance = 1e-15)
    expect_equal(norm(result), sqrt(2), tolerance = 1e-15)
})

# Test error conditions for arithmetic operations
test_that("arithmetic operations error handling", {
    x <- as(c(1, 2, 3), "sparse_numeric")
    y <- as(c(1, 2), "sparse_numeric")

    expect_error(sparse_add(x, y), "vectors must have the same length")
    expect_error(sparse_mult(x, y), "vectors must have the same length")
    expect_error(sparse_sub(x, y), "vectors must have the same length")
    expect_error(sparse_crossprod(x, y), "vectors must have the same length")
    expect_error(plot(x, y), "Cannot plot vectors of different lengths")
})

# Test sparse_mult with overlapping and non-overlapping positions
test_that("sparse_mult overlapping positions", {
    # No overlap
    x <- as(c(1, 0, 2), "sparse_numeric")
    y <- as(c(0, 3, 0), "sparse_numeric")
    result <- sparse_mult(x, y)
    expect_equal(result@value, numeric(0))
    expect_equal(result@pos, integer(0))

    # Partial overlap
    x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
    y <- as(c(0, 4, 0, 5, 0), "sparse_numeric")
    result <- sparse_mult(x, y)
    expect_equal(length(result@value), 0)  # No positions overlap
})

# Test coercion edge cases
test_that("coercion edge cases", {
    # Empty numeric vector
    empty_num <- numeric(0)
    sparse_empty <- as(empty_num, "sparse_numeric")
    expect_equal(sparse_empty@length, 0)
    expect_equal(length(sparse_empty@value), 0)

    # Single zero
    single_zero <- as(0, "sparse_numeric")
    expect_equal(single_zero@length, 1)
    expect_equal(length(single_zero@value), 0)

    # Very large vector
    large_vec <- c(rep(0, 1000), 1, rep(0, 1000))
    sparse_large <- as(large_vec, "sparse_numeric")
    expect_equal(sparse_large@length, 2001)
    expect_equal(length(sparse_large@value), 1)
    expect_equal(sparse_large@value[1], 1)
})

# Test show method with various cases
test_that("show method comprehensive tests", {
    # All zeros
    x <- as(rep(0, 5), "sparse_numeric")
    expect_output(show(x), "Sparse numeric vector of length 5")
    expect_output(show(x), "All elements are zero")

    # Single non-zero
    x <- as(c(0, 0, 42, 0), "sparse_numeric")
    expect_output(show(x), "Sparse numeric vector of length 4")
    expect_output(show(x), "\\[3\\] = 42")

    # Multiple non-zeros
    x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
    expect_output(show(x), "Non-zero elements:")
    expect_output(show(x), "\\[1\\] = 1")
    expect_output(show(x), "\\[3\\] = 3")
    expect_output(show(x), "\\[5\\] = 5")
})

# Test sparsity with edge cases
test_that("sparsity edge cases", {
    # All non-zero (should be 100%)
    x <- as(c(1, 2, 3, 4, 5), "sparse_numeric")
    expect_equal(sparsity(x), 100)

    # Single non-zero in large vector
    x <- as(c(rep(0, 99), 1), "sparse_numeric")
    expect_equal(sparsity(x), 100/100)  # 1%

    # Half sparse
    x <- as(c(1, 0, 2, 0, 3, 0), "sparse_numeric")
    expect_equal(sparsity(x), 50)
})

# Test cross product with various cases
test_that("sparse_crossprod comprehensive tests", {
    # Same vectors
    x <- as(c(1, 2, 3), "sparse_numeric")
    expect_equal(sparse_crossprod(x, x), 1 + 4 + 9)  # 14

    # Orthogonal vectors (no overlap)
    x <- as(c(1, 0, 2), "sparse_numeric")
    y <- as(c(0, 3, 0), "sparse_numeric")
    expect_equal(sparse_crossprod(x, y), 0)

    # Partial overlap
    x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
    y <- as(c(0, 4, 0, 5, 0), "sparse_numeric")
    expect_equal(sparse_crossprod(x, y), 0)

    # Overlapping positions with different values
    x <- as(c(1, 0, 2), "sparse_numeric")
    y <- as(c(3, 0, 4), "sparse_numeric")
    expect_equal(sparse_crossprod(x, y), 1*3 + 2*4)  # 11
})

# Test that methods work with different signatures
test_that("method dispatch works correctly", {
    x <- as(c(1, 2, 3), "sparse_numeric")
    y <- as(c(4, 5, 6), "sparse_numeric")

    # Test that operators work
    expect_equal(x + y, sparse_add(x, y))
    expect_equal(x * y, sparse_mult(x, y))
    expect_equal(x - y, sparse_sub(x, y))

    # Test that mean works without extra args
    expect_equal(mean(x), mean(x, na.rm = TRUE))
    expect_equal(mean(x), mean(x, trim = 0))

    # Test that norm works without extra args
    expect_equal(norm(x), norm(x, something = "else"))

    # Test that standardize works without extra args
    result1 <- standardize(x)
    result2 <- standardize(x, extra = "arg")
    expect_equal(result1, result2)
})

# Additional tests for 90% coverage

# Test validity function edge cases
test_that("validity function edge cases", {
    # Test with empty sparse vector
    empty_sparse <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
    expect_true(validObject(empty_sparse))

    # Test with single element
    single_sparse <- new("sparse_numeric", value = 1, pos = 1L, length = 1L)
    expect_true(validObject(single_sparse))
})

# Test standardize with edge cases
test_that("standardize edge cases", {
    # Test with length 1 (should return empty)
    x <- as(5, "sparse_numeric")
    result <- standardize(x)
    expect_equal(result@value, numeric(0))
    expect_equal(result@length, 1)

    # Test with length 0
    x <- as(numeric(0), "sparse_numeric")
    result <- standardize(x)
    expect_equal(result@length, 0)

    # Test with very small numbers
    x <- as(c(1e-10, 2e-10, 3e-10), "sparse_numeric")
    result <- standardize(x)
    expect_equal(mean(result), 0, tolerance = 1e-15)
    expect_equal(norm(result), sqrt(2), tolerance = 1e-10)
})

# Test plot method more thoroughly
test_that("plot method works", {
    x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
    y <- as(c(0, 4, 0, 5, 0), "sparse_numeric")

    # Should not error
    expect_silent(plot(x, y))
})

# Test coercion with more edge cases
test_that("coercion comprehensive tests", {
    # Test coercion from sparse to numeric with empty vector
    empty_sparse <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 0L)
    empty_dense <- as(empty_sparse, "numeric")
    expect_equal(length(empty_dense), 0)

    # Test coercion from numeric with very large numbers
    large_num <- c(1e100, 0, -1e100, 0, 2e100)
    sparse_large <- as(large_num, "sparse_numeric")
    dense_back <- as(sparse_large, "numeric")
    expect_equal(dense_back, large_num)
})

# Test arithmetic operations with empty results
test_that("arithmetic operations with empty results", {
    # sparse_mult that results in all zeros
    x <- as(c(1, 0, 2), "sparse_numeric")
    y <- as(c(0, 3, 0), "sparse_numeric")
    result <- x * y
    expect_equal(result@value, numeric(0))
    expect_equal(result@pos, integer(0))
    expect_equal(result@length, 3)

    # sparse_add that results in zeros (should not happen with current implementation but test edge case)
    x <- as(c(1, 0), "sparse_numeric")
    y <- as(c(-1, 0), "sparse_numeric")
    result <- x + y
    expect_equal(result@value, numeric(0))  # The sum at position 1 becomes 0
    expect_equal(result@length, 2)
})

# Test show method with empty vector
test_that("show method with empty vector", {
    empty_sparse <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
    expect_output(show(empty_sparse), "Sparse numeric vector of length 5")
    expect_output(show(empty_sparse), "All elements are zero")
})

# Test norm with very small numbers
test_that("norm with very small numbers", {
    x <- as(c(1e-20, 0, 2e-20), "sparse_numeric")
    result <- norm(x)
    expect_equal(result, sqrt(1e-20^2 + 2e-20^2))
})

# Test mean with very large numbers
test_that("mean with very large numbers", {
    x <- as(c(1e100, 0, 2e100), "sparse_numeric")
    result <- mean(x)
    expect_equal(result, (1e100 + 2e100) / 3)
})

# Test sparsity with decimal percentages
test_that("sparsity with decimal percentages", {
    x <- as(c(1, 0, 0, 0, 0, 0, 0), "sparse_numeric")  # 1 out of 7 = 14.2857%
    result <- sparsity(x)
    expect_equal(result, 100/7, tolerance = 1e-10)
})