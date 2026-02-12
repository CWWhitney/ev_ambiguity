# R Function Style Guide for `evca` Package

## Overview

This style guide defines standards for writing R functions in the `evca` package and similar projects. All functions should be well-documented, include examples, handle errors gracefully, and follow roxygen2 documentation standards.

## Template Structure

```r
#' Function Title (Short, Descriptive)
#'
#' Detailed description of what the function does. This should be 1-3 sentences
#' explaining the purpose and main functionality. Include context about when
#' this function would be used.
#'
#' @usage function_name(param1, param2 = default_value)
#'
#' @param param1 Description of first parameter. Include the expected data type,
#'   structure (e.g., "numeric vector", "matrix where rows are X and columns are Y"),
#'   and any constraints (e.g., "must be positive", "must sum to 1").
#' @param param2 Description of second parameter with default value.
#'   Use multiple lines with 2-space indent for long descriptions.
#'
#' @return Description of what the function returns. For complex returns (lists,
#'   data frames), use itemized format:
#'   \item{component1}{Description of first component}
#'   \item{component2}{Description of second component}
#'
#' @details Optional section for additional technical details, algorithms used,
#'   computational complexity, or extended explanations.
#'
#' @section Warning:
#' Important warnings or gotchas users should know about.
#'
#' @references
#' Author, A. (Year). Title of paper. Journal Name, Volume(Issue), pages.
#' @references
#' Author, B. (Year). Another reference if needed.
#'
#' @keywords keyword1 keyword2 keyword3
#'
#' @importFrom package function1 function2
#' @import package_name
#'
#' @examples
#' # Example 1: Basic usage
#' result <- function_name(param1 = value1)
#' print(result)
#'
#' # Example 2: With optional parameters
#' result2 <- function_name(param1 = value1, param2 = value2)
#'
#' # Example 3: Real-world scenario
#' data <- matrix(c(1, 2, 3, 4), nrow = 2)
#' result3 <- function_name(data)
#'
#' @export
#'
function_name <- function(param1, param2 = default_value) {
  
  # Input validation ----
  # Check parameter types
  if (!is.numeric(param1)) {
    stop("param1 must be numeric")
  }
  
  # Check parameter constraints
  if (any(param1 < 0)) {
    stop("param1 must be non-negative")
  }
  
  # Handle missing values
  if (any(is.na(param1))) {
    warning("param1 contains NA values which will be removed")
    param1 <- param1[!is.na(param1)]
  }
  
  # Computation ----
  result <- # ... your computation here
  
  # Return ----
  return(result)
}
```

## Documentation Components

### 1. Title and Description

**Title**: One line, capitalized, no period
```r
#' Expected Value of Eliminating Causal Ambiguity (EVECA)
```

**Description**: 1-3 sentences explaining purpose
```r
#' Calculates the Expected Value of Eliminating Causal Ambiguity using
#' Bayesian model averaging framework. This function quantifies the value
#' of resolving structural uncertainty about which causal model is correct.
```

### 2. Usage (Optional but Recommended)

Show the function signature with defaults:
```r
#' @usage compute_evca(model_utilities, model_probs, utility_function = identity)
```

### 3. Parameters (@param)

Each parameter must be documented:

```r
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must sum to 1 (within floating point tolerance) and be non-negative.
#' @param utility_function Function to apply to outcomes (default: identity).
#'   If provided, will be applied to model_utilities assuming they represent
#'   outcomes rather than utilities.
```

**Guidelines:**
- Start with data type (numeric vector, matrix, data frame, etc.)
- Describe structure (rows/columns, dimensions)
- Include constraints (must sum to 1, positive values, etc.)
- Mention default values and their meaning
- Use 2-space indent for continuation lines

### 4. Return Value (@return)

Describe what the function returns:

**Simple return:**
```r
#' @return Numeric vector of BMA expected utilities for each decision.
```

**Complex return (list):**
```r
#' @return A list with the following components:
#'   \item{evca}{Expected Value of Eliminating Causal Ambiguity.}
#'   \item{optimal_decision_bma}{Integer index of optimal decision under BMA.}
#'   \item{optimal_utility_bma}{Expected utility of optimal BMA decision.}
#'   \item{model_probs}{Original model probabilities (for reference).}
```

### 5. Details (@details)

Optional extended explanation:
```r
#' @details
#' This function implements Bayesian Model Averaging (BMA) to compute
#' expected utilities under structural uncertainty. The EVECA metric
#' represents the maximum amount one should be willing to pay to
#' definitively identify the correct causal model.
#'
#' Computational complexity is O(D × M) where D is the number of decisions
#' and M is the number of models.
```

### 6. Warnings (@section Warning)

Critical information users must know:
```r
#' @section Warning:
#' Model probabilities must sum to exactly 1 (within 1e-10 tolerance).
#' The function will throw an error if this constraint is violated.
#' All utility values should be on the same scale for meaningful comparison.
```

### 7. References (@references)

Academic citations (can have multiple):
```r
#' @references
#' Claxton, K. (1999). The irrelevance of inference: a decision-making 
#' approach to the stochastic evaluation of health care technologies. 
#' Journal of Health Economics, 18(3), 341-364.
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
```

### 8. Keywords (@keywords)

Help with categorization:
```r
#' @keywords models decision-analysis value-of-information
```

Common keywords: `arith`, `math`, `logic`, `methods`, `misc`, `survey`, `models`, `utilities`

### 9. Imports (@importFrom, @import)

Declare package dependencies:
```r
#' @importFrom stats rnorm runif
#' @importFrom dplyr filter select
#' @import ggplot2
```

**Guidelines:**
- Use `@importFrom` when importing specific functions
- Use `@import` when importing entire package (use sparingly)
- List in alphabetical order

### 10. Examples (@examples)

**REQUIRED** - Provide working examples:

```r
#' @examples
#' # Example 1: Basic usage with simple data
#' model_utilities <- matrix(
#'   c(10, 8, 12, 9,
#'     7, 9, 6, 8,
#'     11, 10, 8, 10),
#'   nrow = 3, ncol = 4, byrow = TRUE
#' )
#' model_probs <- c(0.3, 0.25, 0.25, 0.2)
#' 
#' result <- compute_evca(model_utilities, model_probs)
#' print(result$evca)
#' 
#' # Example 2: Using built-in data generator
#' example_data <- example_evca()
#' result <- compute_evca(example_data$model_utilities, 
#'                        example_data$model_probs)
#' 
#' # Example 3: With custom utility function
#' result_risk_averse <- compute_evca(
#'   model_utilities,
#'   model_probs,
#'   utility_function = function(x) -exp(-0.1 * x)
#' )
```

**Example Guidelines:**
- Include at least 2-3 examples
- Start simple, then show advanced usage
- Add comments explaining what each example demonstrates
- Ensure all examples run without errors
- Show realistic use cases
- Keep lines under 100 characters
- For long-running examples, use `\dontrun{}` or `\donttest{}`

### 11. Export (@export)

Make function available to users:
```r
#' @export
```

Or for S3 methods:
```r
#' @export function_name
```

## Function Body Structure

### 1. Input Validation

Always validate inputs at the start:

```r
function_name <- function(param1, param2) {
  
  # Input validation ----
  
  # Check parameter types
  if (!is.numeric(param1)) {
    stop("param1 must be numeric")
  }
  
  if (!is.matrix(param2) && !is.data.frame(param2)) {
    stop("param2 must be a matrix or data frame")
  }
  
  # Convert to standard format if needed
  if (!is.matrix(param2)) {
    param2 <- as.matrix(param2)
  }
  
  # Check dimensions
  if (ncol(param2) != length(param1)) {
    stop(
      "Number of columns in param2 (", ncol(param2), ") ",
      "must equal length of param1 (", length(param1), ")"
    )
  }
  
  # Check constraints (order matters!)
  if (any(param1 < 0)) {
    stop("param1 must be non-negative")
  }
  
  if (abs(sum(param1) - 1) > 1e-10) {
    stop("param1 must sum to 1 (within 1e-10 tolerance)")
  }
  
  # Handle missing values
  if (any(is.na(param2))) {
    warning("param2 contains NA values which will be removed")
    param2 <- param2[stats::complete.cases(param2), ]
  }
  
  # Rest of function...
}
```

**Validation Order:**
1. Check types (is.numeric, is.matrix, etc.)
2. Convert formats if acceptable (as.matrix, as.data.frame)
3. Check dimensions and structure
4. Check value constraints (positive, in range, etc.)
5. Check mathematical constraints (sum to 1, etc.)
6. Handle missing values

### 2. Variable Initialization

Set variables to NULL to avoid R CMD check notes:
```r
  # Set variables to NULL first, appeasing R CMD check
  result <- optimal_idx <- utility_values <- NULL
```

### 3. Main Computation

Use clear section headers with `----`:
```r
  # Compute BMA expected utility ----
  bma_eu <- as.vector(model_utilities %*% model_probs)
  
  # Find optimal decision ----
  optimal_decision <- which.max(bma_eu)
  optimal_utility <- bma_eu[optimal_decision]
```

### 4. Return Values

Always use explicit return:
```r
  # Return results ----
  return(list(
    evca = evca,
    optimal_decision = optimal_decision,
    optimal_utility = optimal_utility
  ))
```

## Error Handling Best Practices

### Stop vs Warning vs Message

**Use `stop()` for:**
- Invalid input types
- Constraint violations that make computation impossible
- Missing required parameters

```r
if (!is.numeric(x)) {
  stop("x must be numeric")
}
```

**Use `warning()` for:**
- Issues that can be handled but user should know about
- Data transformations that might affect results
- Removed values (NA, infinite, etc.)

```r
if (any(is.na(data))) {
  warning("data contains NA values which were removed")
  data <- data[!is.na(data)]
}
```

**Use `message()` for:**
- Informational output
- Progress indicators for long operations

```r
message("Computing EVCA for ", n_decisions, " decisions and ", n_models, " models...")
```

### Informative Error Messages

**Bad:**
```r
stop("Invalid input")
```

**Good:**
```r
stop(
  "Number of model probabilities (", length(model_probs), ") ",
  "must equal number of model columns (", ncol(model_utilities), ")"
)
```

Include:
- What's wrong
- Current values
- Expected values
- How to fix it

## Complete Example Following All Guidelines

```r
#' Expected Value of Eliminating Causal Ambiguity (EVECA)
#'
#' Computes the Expected Value of Eliminating Causal Ambiguity using
#' Bayesian model averaging. EVECA quantifies the maximum value of
#' resolving structural uncertainty about which causal model is correct.
#'
#' @usage compute_evca(model_utilities, model_probs, utility_function = identity)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must sum to 1 (within floating point tolerance) and be non-negative.
#' @param utility_function Function to apply to outcomes (default: identity).
#'   If provided, will be applied to model_utilities assuming they represent
#'   outcomes rather than utilities.
#'
#' @return A list with the following components:
#'   \item{evca}{Expected Value of Eliminating Causal Ambiguity.}
#'   \item{optimal_decision_bma}{Integer index of optimal decision under BMA.}
#'   \item{optimal_utility_bma}{Expected utility of optimal BMA decision.}
#'   \item{perfect_info_expected_utility}{Expected utility with perfect information.}
#'   \item{model_probs}{Original model probabilities (for reference).}
#'
#' @details
#' The function implements Bayesian Model Averaging (BMA) to compute expected
#' utilities under structural uncertainty. EVECA is calculated as the difference
#' between expected utility with perfect structural information and expected
#' utility under current ambiguity.
#'
#' Computational complexity is O(D × M) where D is the number of decisions
#' and M is the number of models.
#'
#' @section Warning:
#' Model probabilities must sum to exactly 1 (within 1e-10 tolerance).
#' All utility values should be on the same scale for meaningful comparison.
#'
#' @references
#' Claxton, K. (1999). The irrelevance of inference: a decision-making 
#' approach to the stochastic evaluation of health care technologies. 
#' Journal of Health Economics, 18(3), 341-364.
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#'
#' @keywords models decision-analysis value-of-information
#'
#' @examples
#' # Example 1: Basic usage with 3 decisions and 4 models
#' model_utilities <- matrix(
#'   c(10, 8, 12, 9,
#'     7, 9, 6, 8,
#'     11, 10, 8, 10),
#'   nrow = 3, ncol = 4, byrow = TRUE,
#'   dimnames = list(
#'     paste("Decision", 1:3),
#'     paste("Model", 1:4)
#'   )
#' )
#' model_probs <- c(0.3, 0.25, 0.25, 0.2)
#' 
#' result <- compute_evca(model_utilities, model_probs)
#' print(result$evca)
#' print(result$optimal_decision_bma)
#'
#' # Example 2: Using example data generator
#' example_data <- example_evca()
#' result <- compute_evca(
#'   example_data$model_utilities,
#'   example_data$model_probs
#' )
#'
#' # Example 3: With risk-averse utility function
#' result_risk <- compute_evca(
#'   model_utilities,
#'   model_probs,
#'   utility_function = function(x) -exp(-0.1 * x)
#' )
#' 
#' # Compare risk-neutral vs risk-averse
#' c(risk_neutral = result$evca, risk_averse = result_risk$evca)
#'
#' @export
#'
compute_evca <- function(model_utilities, 
                         model_probs,
                         utility_function = identity) {
  
  # Input validation ----
  
  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    model_utilities <- as.matrix(model_utilities)
  }
  
  # Apply utility function if provided
  if (!identical(utility_function, identity)) {
    model_utilities <- apply(model_utilities, c(1, 2), utility_function)
  }
  
  # Get dimensions
  n_decisions <- nrow(model_utilities)
  n_models <- ncol(model_utilities)
  
  # Validate dimensions
  if (length(model_probs) != n_models) {
    stop(
      "Number of model probabilities (", length(model_probs), ") ",
      "must equal number of model columns (", n_models, ")"
    )
  }
  
  # Validate probability constraints (order matters!)
  if (any(model_probs < 0)) {
    stop("Model probabilities must be non-negative")
  }
  
  if (abs(sum(model_probs) - 1) > 1e-10) {
    stop("Model probabilities must sum to 1 (within 1e-10 tolerance)")
  }
  
  # Compute BMA expected utility ----
  bma_eu <- as.vector(model_utilities %*% model_probs)
  
  # Find optimal decision under BMA ----
  optimal_decision_bma <- which.max(bma_eu)
  optimal_utility_bma <- bma_eu[optimal_decision_bma]
  
  # Compute perfect information expected utility ----
  optimal_utilities_per_model <- apply(model_utilities, 2, max)
  perfect_info_eu <- sum(model_probs * optimal_utilities_per_model)
  
  # Calculate EVCA ----
  evca <- perfect_info_eu - optimal_utility_bma
  
  # Return results ----
  return(list(
    evca = evca,
    optimal_decision_bma = optimal_decision_bma,
    optimal_utility_bma = optimal_utility_bma,
    perfect_info_expected_utility = perfect_info_eu,
    model_probs = model_probs,
    n_decisions = n_decisions,
    n_models = n_models
  ))
}
```

## Checklist for New Functions

- [ ] Title is clear and descriptive
- [ ] Description explains what and why (1-3 sentences)
- [ ] All parameters documented with `@param`
- [ ] Parameter descriptions include type, structure, and constraints
- [ ] Return value fully documented
- [ ] At least 2-3 working examples provided
- [ ] Examples demonstrate basic and advanced usage
- [ ] Input validation checks types
- [ ] Input validation checks dimensions
- [ ] Input validation checks constraints (in correct order)
- [ ] Error messages are informative with actual values
- [ ] Warnings for data transformations
- [ ] Keywords added for discoverability
- [ ] References included if based on published methods
- [ ] Function exported with `@export`
- [ ] Package dependencies declared with `@importFrom` or `@import`
- [ ] Function tested and all examples run
- [ ] Follows naming conventions (snake_case for functions)
- [ ] Code includes section headers with `----`

## Additional Resources

- [roxygen2 documentation](https://roxygen2.r-lib.org/)
- [R Packages book](https://r-pkgs.org/)
- [Tidyverse style guide](https://style.tidyverse.org/)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/R-exts.html)
