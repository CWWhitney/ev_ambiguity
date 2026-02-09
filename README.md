# evca: Expected Value of Eliminating Causal Ambiguity

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`evca` provides tools for computing the **Expected Value of Eliminating Causal Ambiguity (EVCA)**, a decision-theoretic framework that quantifies the value of resolving structural uncertainty when multiple competing causal models exist.

Traditional value of information (VOI) analysis addresses parameter uncertainty within a fixed causal model. EVCA extends VOI to handle **causal ambiguity**—situations where you're uncertain about which causal model is correct among several competing alternatives.

### Key Question

**Should you act now under uncertainty, or invest in research to identify the correct causal model?**

EVCA provides a quantitative answer by computing the maximum value of resolving model uncertainty.

## Installation

```r
# Install from GitHub
devtools::install_github("corywhitney/evca")

# Or install locally
devtools::install_local("path/to/evca")
```

## Quick Start

```r
library(evca)

# Define utility matrix: rows = decisions, columns = models
model_utilities <- matrix(
  c(8.5, 6.0, 9.0, 7.5,   # Decision 1 under each model
    7.0, 8.5, 5.5, 8.0,   # Decision 2 under each model
    9.0, 7.5, 6.5, 9.5),  # Decision 3 under each model
  nrow = 3, ncol = 4, byrow = TRUE
)

# Model probabilities (from data or expert judgment)
model_probs <- c(0.35, 0.30, 0.20, 0.15)

# Compute EVCA
result <- compute_evca(model_utilities, model_probs)

# View results
print(result$evca)  # Value of resolving ambiguity
print(result$optimal_decision_bma)  # Optimal decision under uncertainty
```

## Core Concepts

### Causal Ambiguity

**Causal ambiguity** occurs when multiple competing theories exist about how an intervention affects outcomes:

- Policy might work through direct effects, spillovers, or both
- Treatment might operate via mechanism A, B, or a combination
- Agricultural practice might improve yields through soil health, pest control, or water retention

### Bayesian Model Averaging (BMA)

Under ambiguity, we use BMA to compute expected utility:

```
EU_BMA(decision) = Σ P(model) × EU(decision|model)
```

### EVCA Formula

EVCA quantifies the value of perfect structural information:

```
EVCA = E[max utility with perfect info] - max[E[utility under ambiguity]]
     = Σ P(model) × max_d EU(d|model) - max_d Σ P(model) × EU(d|model)
```

### Interpretation

- **High EVCA**: Worth investing in research to identify correct model
- **Low EVCA**: Optimal decision is robust across models; proceed without additional research

## Features

### Core Functions

- `compute_evca()`: Calculate EVCA given model utilities and probabilities
- `bma_expected_utility()`: Compute Bayesian model averaging
- `example_evca()`: Generate example datasets for testing
- `example_evca_multidim()`: Generate examples with multiple outcome dimensions

### Visualization

- `plot_evca()`: Basic EVCA visualization
- `plot_utilities_heatmap()`: Heatmap of decision-model utilities
- `plot_sensitivity()`: Sensitivity analysis for model probabilities

### Multi-Dimensional Outcomes

Handle decisions with multiple outcomes using multi-attribute utility:

```r
# Generate example with 2 outcome dimensions
result <- example_evca_multidim(
  n_decisions = 3,
  n_models = 4,
  n_outcomes = 2,
  outcome_weights = c(0.6, 0.4)  # Trade-off weights
)
```

## Example Workflow

### 1. Generate or Define Your Data

```r
# Use example generator
example_data <- example_evca(n_decisions = 4, n_models = 3)

# Or define your own utilities
model_utilities <- matrix(...)
model_probs <- c(...)
```

### 2. Compute EVCA

```r
result <- compute_evca(model_utilities, model_probs)

cat("EVCA:", result$evca, "\n")
cat("Optimal decision:", result$optimal_decision_bma, "\n")
```

### 3. Visualize Results

```r
# Basic plot
plot_evca(result)

# Utility heatmap
plot_utilities_heatmap(model_utilities, model_probs, 
                       optimal_decision = result$optimal_decision_bma)

# Sensitivity analysis
plot_sensitivity(model_utilities, model_probs, vary_model = 1)
```

### 4. Make Decision

```r
research_cost <- 2.0

if (result$evca > research_cost) {
  cat("Decision: INVEST IN RESEARCH to identify correct model\n")
} else {
  cat("Decision: PROCEED with decision", result$optimal_decision_bma, "\n")
  cat("Rationale: EVCA < research cost, decision is robust\n")
}
```

## Mathematical Details

### Setup

Given:
- K competing causal models: M₁, M₂, ..., Mₖ
- Model probabilities: p(M₁), p(M₂), ..., p(Mₖ)
- D decision alternatives: d₁, d₂, ..., dᴰ
- Expected utility EU(dᵢ|Mⱼ) for decision i under model j

### Under Ambiguity (BMA)

The expected utility for each decision:
```
EU_BMA(d) = Σₖ p(Mₖ) × EU(d|Mₖ)
```

Optimal decision: `d* = argmax_d EU_BMA(d)`

### With Perfect Information

If we knew which model is true, expected utility would be:
```
EU_perfect = Σₖ p(Mₖ) × max_d EU(d|Mₖ)
```

### EVCA

The difference:
```
EVCA = EU_perfect - EU_BMA(d*)
```

This represents the maximum we should pay to definitively identify the correct causal model.

## Advanced Features

### Risk Preferences

Incorporate risk aversion via utility functions:

```r
# Risk-neutral (default)
result_neutral <- compute_evca(outcomes, model_probs)

# Risk-averse (exponential utility)
result_averse <- compute_evca(
  outcomes, 
  model_probs,
  utility_function = function(x) -exp(-0.02 * x)
)
```

### Multi-Attribute Utility

For decisions affecting multiple outcomes:

```r
# Create outcome array: decisions × models × outcomes
model_outcomes <- array(...)

# Define weights for outcomes
outcome_weights <- c(0.6, 0.4)  # Must sum to 1

# Compute multi-attribute utilities
model_utilities <- compute_multi_attribute_utilities(
  model_outcomes, 
  outcome_weights
)

# Then compute EVCA as usual
result <- compute_evca(model_utilities, model_probs)
```

## Use Cases

EVCA is applicable wherever causal ambiguity exists:

- **Policy evaluation**: Different theories about how policy works
- **Medical decisions**: Multiple biological mechanisms for treatment effects
- **Agriculture**: Competing pathways for yield improvements
- **Environmental policy**: Alternative models of ecosystem response
- **Technology adoption**: Different diffusion mechanisms
- **Organizational interventions**: Multiple theories of behavioral change

## Citation

If you use this package, please cite:

```
Whitney, C. (2024). evca: Expected Value of Eliminating Causal Ambiguity. 
R package version 0.3.0. https://github.com/corywhitney/evca
```

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## References

- **Value of Information**: Claxton (1999), Yokota & Thompson (2004)
- **Bayesian Model Averaging**: Hoeting et al. (1999)
- **Model Uncertainty**: Brock et al. (2003)

## Support

For issues and feature requests, please use the [GitHub issue tracker](https://github.com/corywhitney/evca/issues).