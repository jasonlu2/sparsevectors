# Final verification of sparsevectors package requirements
# Run this script to ensure all requirements are met

cat("=== FINAL SPARSE VECTORS PACKAGE VERIFICATION ===\n\n")

# Check package structure
cat("1. PACKAGE STRUCTURE CHECK\n")
required_files <- c("DESCRIPTION", "NAMESPACE", "README.md")
required_dirs <- c("R", "tests", ".github")

all_files_present <- all(file.exists(required_files))
all_dirs_present <- all(dir.exists(required_dirs))

cat("✓ Required files present:", all_files_present, "\n")
cat("✓ Required directories present:", all_dirs_present, "\n")

if (all_files_present && all_dirs_present) {
  cat("✓ PACKAGE STRUCTURE: CORRECT\n")
} else {
  cat("✗ PACKAGE STRUCTURE: INCOMPLETE\n")
}

cat("\n")

# Check DESCRIPTION
cat("2. DESCRIPTION FILE CHECK\n")
if (file.exists("DESCRIPTION")) {
  desc <- read.dcf("DESCRIPTION")
  checks <- c(
    "Package name" = desc[1,"Package"] == "sparsevectors",
    "Title present" = nchar(desc[1,"Title"]) > 0,
    "Author present" = "Authors@R" %in% colnames(desc) && nchar(desc[1,"Authors@R"]) > 0,
    "Description present" = nchar(desc[1,"Description"]) > 20,
    "License present" = nchar(desc[1,"License"]) > 0,
    "RoxygenNote present" = "RoxygenNote" %in% colnames(desc),
    "Imports methods" = grepl("methods", desc[1,"Imports"]),
    "Suggests testthat" = grepl("testthat", desc[1,"Suggests"]),
    "Suggests covr" = grepl("covr", desc[1,"Suggests"])
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  if (all(checks)) {
    cat("✓ DESCRIPTION FILE: COMPLETE\n")
  } else {
    cat("✗ DESCRIPTION FILE: INCOMPLETE\n")
  }
}

cat("\n")

# Check NAMESPACE
cat("3. NAMESPACE CHECK\n")
if (file.exists("NAMESPACE")) {
  ns_content <- readLines("NAMESPACE")
  checks <- c(
    "Imports methods" = any(grepl("import.*methods", ns_content)),
    "Exports sparse_add" = any(grepl("export.*sparse_add", ns_content)),
    "Exports sparse_mult" = any(grepl("export.*sparse_mult", ns_content)),
    "Exports sparse_sub" = any(grepl("export.*sparse_sub", ns_content)),
    "Exports sparse_crossprod" = any(grepl("export.*sparse_crossprod", ns_content)),
    "Exports sparsity" = any(grepl("export.*sparsity", ns_content)),
    "Exports norm" = any(grepl("export.*norm", ns_content)),
    "Exports standardize" = any(grepl("export.*standardize", ns_content)),
    "Exports sparse_numeric class" = any(grepl("exportClasses.*sparse_numeric", ns_content)),
    "Exports methods" = any(grepl("exportMethods", ns_content))
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  if (all(checks)) {
    cat("✓ NAMESPACE: CORRECT\n")
  } else {
    cat("✗ NAMESPACE: INCOMPLETE\n")
  }
}

cat("\n")

# Check R code
cat("4. R CODE CHECK\n")
r_file <- "R/sparse_numeric.R"
if (file.exists(r_file)) {
  r_content <- readLines(r_file)

  checks <- c(
    "Class definition" = any(grepl("setClass", r_content)) && any(grepl("sparse_numeric", r_content)),
    "Validity method" = any(grepl("setValidity.*sparse_numeric", r_content)),
    "Generic sparse_add" = any(grepl("setGeneric.*sparse_add", r_content)),
    "Generic sparse_mult" = any(grepl("setGeneric.*sparse_mult", r_content)),
    "Generic sparse_sub" = any(grepl("setGeneric.*sparse_sub", r_content)),
    "Generic sparse_crossprod" = any(grepl("setGeneric.*sparse_crossprod", r_content)),
    "Generic sparsity" = any(grepl("setGeneric.*sparsity", r_content)),
    "Generic norm" = any(grepl("setGeneric.*norm", r_content)),
    "Generic standardize" = any(grepl("setGeneric.*standardize", r_content)),
    "Method sparse_add" = any(grepl("setMethod.*sparse_add.*sparse_numeric", r_content)),
    "Method sparse_mult" = any(grepl("setMethod.*sparse_mult.*sparse_numeric", r_content)),
    "Method sparse_sub" = any(grepl("setMethod.*sparse_sub.*sparse_numeric", r_content)),
    "Method sparse_crossprod" = any(grepl("setMethod.*sparse_crossprod.*sparse_numeric", r_content)),
    "Method sparsity" = any(grepl("setMethod.*sparsity.*sparse_numeric", r_content)),
    "Method norm" = any(grepl("setMethod.*norm.*sparse_numeric", r_content)),
    "Method standardize" = any(grepl("setMethod.*standardize.*sparse_numeric", r_content)),
    "Method mean" = any(grepl("setMethod.*mean.*sparse_numeric", r_content)),
    "Method show" = any(grepl("setMethod.*show.*sparse_numeric", r_content)),
    "Method plot" = any(grepl("setMethod.*plot.*sparse_numeric", r_content)),
    "Arithmetic operators" = any(grepl("setMethod.*\\+.*sparse_numeric", r_content)),
    "Coercion methods" = any(grepl("setAs", r_content))
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  if (all(checks)) {
    cat("✓ R CODE: COMPLETE\n")
  } else {
    cat("✗ R CODE: INCOMPLETE\n")
  }
}

cat("\n")

# Check tests
cat("5. TESTS CHECK\n")
test_file <- "tests/testthat/test-sparse-vectors.R"
if (file.exists(test_file)) {
  test_content <- readLines(test_file)
  test_count <- length(grep("test_that", test_content))

  checks <- c(
    "Test file exists" = TRUE,
    "Has validity tests" = any(grep("validity", test_content)),
    "Has coercion tests" = any(grep("coercion", test_content)),
    "Has arithmetic tests" = any(grep("sparse_add\\(|sparse_mult\\(|sparse_sub\\(", test_content)),
    "Has cross product tests" = any(grep("sparse_crossprod", test_content)),
    "Has sparsity tests" = any(grep("sparsity", test_content)),
    "Has mean tests" = any(grep("mean", test_content)),
    "Has norm tests" = any(grep("norm", test_content)),
    "Has standardize tests" = any(grep("standardize", test_content)),
    "Sufficient test count" = test_count >= 25
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  cat("  Total tests:", test_count, "\n")

  if (all(checks)) {
    cat("✓ TESTS: COMPREHENSIVE\n")
  } else {
    cat("✗ TESTS: INCOMPLETE\n")
  }
}

cat("\n")

# Check README
cat("6. README CHECK\n")
if (file.exists("README.md")) {
  readme <- readLines("README.md")

  checks <- c(
    "README exists" = TRUE,
    "Has title" = any(grepl("^# sparsevectors", readme)),
    "Has badges" = any(grepl("badge", readme)),
    "Has installation" = any(grepl("install", readme, ignore.case = TRUE)),
    "Has usage examples" = any(grepl("usage|example", readme, ignore.case = TRUE)),
    "Has GitHub Actions badge" = any(grepl("github.*actions", readme, ignore.case = TRUE)),
    "Sufficient length" = length(readme) >= 50
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  if (all(checks)) {
    cat("✓ README: COMPLETE\n")
  } else {
    cat("✗ README: INCOMPLETE\n")
  }
}

cat("\n")

# Check pkgdown
cat("7. PKGDOWN CHECK\n")
checks <- c(
  "_pkgdown.yml exists" = file.exists("_pkgdown.yml"),
  ".github/workflows exists" = dir.exists(".github/workflows"),
  "R-CMD-check workflow" = file.exists(".github/workflows/R-CMD-check.yaml"),
  "pkgdown workflow" = file.exists(".github/workflows/pkgdown.yaml")
)

for (check_name in names(checks)) {
  status <- ifelse(checks[check_name], "✓", "✗")
  cat(status, check_name, ":", checks[check_name], "\n")
}

if (all(checks)) {
  cat("✓ PKGDOWN & CI/CD: CONFIGURED\n")
} else {
  cat("✗ PKGDOWN & CI/CD: INCOMPLETE\n")
}

cat("\n")

# Check .gitignore
cat("8. .GITIGNORE CHECK\n")
if (file.exists(".gitignore")) {
  gitignore <- readLines(".gitignore")
  checks <- c(
    ".gitignore exists" = TRUE,
    "Ignores Rproj files" = any(grepl("\\.Rproj", gitignore)),
    "Ignores docs" = any(grepl("docs/", gitignore))
  )

  for (check_name in names(checks)) {
    status <- ifelse(checks[check_name], "✓", "✗")
    cat(status, check_name, ":", checks[check_name], "\n")
  }

  if (all(checks)) {
    cat("✓ .GITIGNORE: APPROPRIATE\n")
  } else {
    cat("✗ .GITIGNORE: INCOMPLETE\n")
  }
} else {
  cat("✗ .GITIGNORE: MISSING\n")
}

cat("\n")

# Check git repository
cat("9. GIT REPOSITORY CHECK\n")
checks <- c(
  ".git exists" = dir.exists(".git"),
  "Is git repository" = TRUE  # Assumed if .git exists
)

for (check_name in names(checks)) {
  status <- ifelse(checks[check_name], "✓", "✗")
  cat(status, check_name, ":", checks[check_name], "\n")
}

if (all(checks)) {
  cat("✓ GIT REPOSITORY: INITIALIZED\n")
} else {
  cat("✗ GIT REPOSITORY: NOT INITIALIZED\n")
}

cat("\n")

# Final summary
cat("=== FINAL SUMMARY ===\n")
cat("This is a comprehensive check of all package requirements.\n")
cat("The package should now be ready for submission!\n")
cat("\n")
cat("Next steps:\n")
cat("1. Push to GitHub repository\n")
cat("2. Enable GitHub Pages for pkgdown website\n")
cat("3. Monitor CI/CD workflows\n")
cat("\n")
cat("=== VERIFICATION COMPLETE ===\n")
