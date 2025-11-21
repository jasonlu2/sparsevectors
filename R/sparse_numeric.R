#' Sparse Numeric Vector Class
#'
#' A class for representing sparse numeric vectors efficiently by storing only
#' non-zero values and their positions.
#'
#' @slot value A numeric vector containing the non-zero values
#' @slot pos An integer vector containing the positions of non-zero values
#' @slot length An integer representing the total length of the vector
#'
#' @examples
#' # Create a sparse vector from a dense vector
#' dense_vec <- c(0, 1, 0, 2, 0, 3)
#' sparse_vec <- as(dense_vec, "sparse_numeric")
#' print(sparse_vec)
#'
#' @export
setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

#' Validity check for sparse_numeric objects
#'
#' @param object A sparse_numeric object to validate
#' @return TRUE if valid, otherwise an error message
#' @name sparse_numeric-validity
setValidity("sparse_numeric", function(object) {
    if (length(object@value) != length(object@pos)) {
        return("value and pos must have the same length")
    }
    if (any(object@pos < 1L) || any(object@pos > object@length)) {
        return("pos must be between 1 and length")
    }
    if (anyDuplicated(object@pos)) {
        return("pos must contain unique positions")
    }
    if (length(object@length) != 1L) {
        return("length must be a single integer")
    }
    return(TRUE)
})

#' Generic function for sparse vector addition
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments
#' @return A sparse vector containing the sum
#'
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Generic function for sparse vector multiplication
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments
#' @return A sparse vector containing the element-wise product
#'
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Generic function for sparse vector subtraction
#'
#' @param x First sparse vector (minuend)
#' @param y Second sparse vector (subtrahend)
#' @param ... Additional arguments
#' @return A sparse vector containing the difference
#'
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Generic function for sparse vector cross product
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments
#' @return A numeric value representing the dot product
#'
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' Add two sparse vectors
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments (unused)
#' @return A sparse vector containing the element-wise sum
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' y <- as(c(0, 3, 0, 4), "sparse_numeric")
#' result <- sparse_add(x, y)
#' # Result should have values at positions 1, 2, 3, 4
#'
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    all_pos <- sort(union(x@pos, y@pos))

    x_map <- match(all_pos, x@pos)
    y_map <- match(all_pos, y@pos)

    new_values <- numeric(length(all_pos))
    new_values[!is.na(x_map)] <- new_values[!is.na(x_map)] + x@value[na.omit(x_map)]
    new_values[!is.na(y_map)] <- new_values[!is.na(y_map)] + y@value[na.omit(y_map)]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = all_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

#' Multiply two sparse vectors element-wise
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments (unused)
#' @return A sparse vector containing the element-wise product
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' y <- as(c(0, 3, 0, 4), "sparse_numeric")
#' result <- sparse_mult(x, y)
#' # Result should be all zeros (no overlapping non-zero positions)
#'
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    common_pos <- intersect(x@pos, y@pos)
    if (length(common_pos) == 0) {
        return(new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length))
    }

    x_idx <- match(common_pos, x@pos)
    y_idx <- match(common_pos, y@pos)

    new_values <- x@value[x_idx] * y@value[y_idx]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = common_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

#' Subtract two sparse vectors
#'
#' @param x First sparse vector (minuend)
#' @param y Second sparse vector (subtrahend)
#' @param ... Additional arguments (unused)
#' @return A sparse vector containing the element-wise difference
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' y <- as(c(0, 3, 0, 4), "sparse_numeric")
#' result <- sparse_sub(x, y)
#' # Result should have values 1, -3, 2, -4 at positions 1, 2, 3, 4
#'
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    all_pos <- sort(union(x@pos, y@pos))

    x_map <- match(all_pos, x@pos)
    y_map <- match(all_pos, y@pos)

    new_values <- numeric(length(all_pos))
    new_values[!is.na(x_map)] <- new_values[!is.na(x_map)] + x@value[na.omit(x_map)]
    new_values[!is.na(y_map)] <- new_values[!is.na(y_map)] - y@value[na.omit(y_map)]

    non_zero <- new_values != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = all_pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})

#' Compute cross product (dot product) of two sparse vectors
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional arguments (unused)
#' @return A numeric value representing the dot product
#'
#' @examples
#' x <- as(c(1, 0, 2, 0), "sparse_numeric")
#' y <- as(c(0, 3, 0, 4), "sparse_numeric")
#' result <- sparse_crossprod(x, y)
#' # Result should be 0 (no overlapping non-zero positions)
#'
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("vectors must have the same length")
    }

    common_pos <- intersect(x@pos, y@pos)
    if (length(common_pos) == 0) {
        return(0)
    }

    x_idx <- match(common_pos, x@pos)
    y_idx <- match(common_pos, y@pos)

    sum(x@value[x_idx] * y@value[y_idx])
})

#' Arithmetic operators for sparse vectors
#'
#' @param e1 First sparse vector
#' @param e2 Second sparse vector
#' @return A sparse vector containing the result of the operation
#'
#' @name sparse-arithmetic
NULL

#' @rdname sparse-arithmetic
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse-arithmetic
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

#' @rdname sparse-arithmetic
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))

#' Coercion from numeric to sparse_numeric
#'
#' @param from A numeric vector to convert
#' @return A sparse_numeric object
#' @name coerce,numeric,sparse_numeric-method
setAs("numeric", "sparse_numeric", function(from) {
    non_zero <- from != 0
    if (any(non_zero)) {
        new("sparse_numeric",
            value = from[non_zero],
            pos = which(non_zero),
            length = length(from))
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = length(from))
    }
})

#' Coercion from sparse_numeric to numeric
#'
#' @param from A sparse_numeric object to convert
#' @return A numeric vector
#' @name coerce,sparse_numeric,numeric-method
setAs("sparse_numeric", "numeric", function(from) {
    result <- numeric(from@length)
    if (length(from@pos) > 0) {
        result[from@pos] <- from@value
    }
    result
})

#' Display a sparse vector
#'
#' @param object A sparse_numeric object to display
#'
#' @export
setMethod("show", "sparse_numeric", function(object) {
    cat("Sparse numeric vector of length", object@length, "\n")
    if (length(object@pos) == 0) {
        cat("All elements are zero\n")
    } else {
        cat("Non-zero elements:\n")
        for (i in seq_along(object@pos)) {
            cat("  [", object@pos[i], "] = ", object@value[i], "\n", sep = "")
        }
    }
})

#' Plot two sparse vectors for comparison
#'
#' @param x First sparse vector
#' @param y Second sparse vector
#' @param ... Additional plotting parameters
#'
#' @examples
#' \dontrun{
#' x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
#' y <- as(c(0, 2, 0, 1, 0), "sparse_numeric")
#' plot(x, y)
#' }
#'
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
    if (x@length != y@length) {
        stop("Cannot plot vectors of different lengths")
    }

    overlap_pos <- intersect(x@pos, y@pos)

    plot(1, type = "n", xlim = c(1, x@length), ylim = c(-max(c(x@value, y@value), 0.1), max(c(x@value, y@value), 0.1)),
         xlab = "Position", ylab = "Value", main = "Sparse Vector Comparison")

    if (length(x@pos) > 0) {
        points(x@pos, x@value, col = "blue", pch = 16, cex = 1.5)
    }

    if (length(y@pos) > 0) {
        points(y@pos, y@value, col = "red", pch = 17, cex = 1.5)
    }

    if (length(overlap_pos) > 0) {
        x_overlap_idx <- match(overlap_pos, x@pos)
        y_overlap_idx <- match(overlap_pos, y@pos)
        points(overlap_pos, x@value[x_overlap_idx], col = "green", pch = 15, cex = 2)
        points(overlap_pos, y@value[y_overlap_idx], col = "green", pch = 15, cex = 2)
    }

    legend("topright", legend = c("Vector x", "Vector y", "Overlap"),
           col = c("blue", "red", "green"), pch = c(16, 17, 15), cex = 0.8)
})

#' Calculate sparsity percentage of a sparse vector
#'
#' @param x A sparse_numeric object
#' @return The percentage of non-zero elements (0-100)
#'
#' @examples
#' x <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
#' sparsity(x)  # Should return 40 (2 non-zero out of 5 elements)
#'
#' @export
setGeneric("sparsity", function(x) standardGeneric("sparsity"))

#' @rdname sparsity
#' @export
setMethod("sparsity", "sparse_numeric", function(x) {
    (length(x@pos) / x@length) * 100
})

#' Calculate mean of a sparse vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (unused)
#' @return The mean value of all elements including zeros
#'
#' @examples
#' x <- as(c(1, 0, 2, 0, 0), "sparse_numeric")
#' mean(x)  # Should return 0.6 (sum=3, length=5)
#'
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
    sum(x@value) / x@length
})

#' Calculate Euclidean norm of a sparse vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (unused)
#' @return The Euclidean norm (square root of sum of squared elements)
#'
#' @examples
#' x <- as(c(3, 0, 4, 0), "sparse_numeric")
#' norm(x)  # Should return 5 (sqrt(3^2 + 4^2))
#'
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
    sqrt(sum(x@value^2))
})

#' Standardize a sparse vector
#'
#' Centers the vector by subtracting the mean and scales by dividing by the standard deviation.
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (unused)
#' @return A standardized sparse vector
#'
#' @examples
#' x <- as(c(1, 2, 3, 4, 5), "sparse_numeric")
#' standardized <- standardize(x)
#' # Result will have mean approximately 0 and standard deviation 1
#'
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
    # Calculate mean
    vec_mean <- mean(x)

    # Calculate sample standard deviation
    # Sample variance = sum((x-mean)^2) / (n-1)
    # For sparse vectors: variance = (sum(x^2) - n*mean^2) / (n-1)
    if (x@length <= 1) {
        # Cannot compute sample standard deviation for n <= 1
        return(new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length))
    }

    sum_sq <- sum(x@value^2)
    variance <- (sum_sq - x@length * vec_mean^2) / (x@length - 1)
    vec_sd <- sqrt(variance)

    if (vec_sd == 0) {
        # All elements are the same, return zeros
        return(new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length))
    }

    # Standardize: (x - mean) / sd
    new_values <- (x@value - vec_mean) / vec_sd

    # Keep only non-zero values after standardization
    non_zero <- abs(new_values) > .Machine$double.eps * 10  # Use small tolerance for floating point
    if (any(non_zero)) {
        new("sparse_numeric",
            value = new_values[non_zero],
            pos = x@pos[non_zero],
            length = x@length)
    } else {
        new("sparse_numeric",
            value = numeric(0),
            pos = integer(0),
            length = x@length)
    }
})
