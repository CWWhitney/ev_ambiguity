# evca: decision diagnostics for contested causal models

`evca` computes a weighted aggregation shortfall and leave-one-model-out
exclusion costs from a decision-by-model utility matrix. The calculation is
relevant to structural uncertainty, but its interpretation depends on what the
weights mean.

- With **epistemic probabilities** over models, the shortfall is EVPI for the
  model index, conditional on the supplied utilities and model set.
- With **evaluative weights** assigning standing to stakeholder accounts, the
  same arithmetic is an aggregation diagnostic. It is not the value of research
  to identify a true model.

The package does not construct causal models, elicit utilities, or justify a
common scale across incommensurable welfare measures. Those choices must be
reported with any result.

## Installation

```r
devtools::install_github("CWWhitney/ev_ambiguity")
```

## Quick start: evaluative weights

Rows are decisions; columns are stakeholder models. Utilities should already
be on a defensible common scale. The example below uses min-max normalization,
which gives each model's realized action range equal stakes.

```r
library(evca)

U_raw <- matrix(
  c(5, 40, 80,
    90, 82, 8,
    72, 78, 55),
  nrow = 3, byrow = TRUE,
  dimnames = list(
    c("No road", "Direct route", "Rerouted road"),
    c("Officials", "ProDev_Hmong", "Traditional_Hmong")
  )
)
U <- normalize_utilities(U_raw)

# These are analyst-declared standing weights, not model probabilities.
w <- c(1/3, 1/3, 1/3)
result <- compute_evca(U, model_weights = w)
result$evca
result$model_contributions
```

`model_contributions` gives the additive contribution of each account to the
shortfall; the contributions sum to `result$evca`. Report the scale and the
process used to choose `w` alongside the number.

## Epistemic probabilities

When the columns are competing states or models and the weights express belief
about which model governs, use `model_probs` instead:

```r
result_epistemic <- compute_evca(U, model_probs = c(0.5, 0.3, 0.2))
```

Under this interpretation, the same formula is expected value of perfect
information for the model index. `compute_evpi_gap()` makes that relationship
explicit. Data updating must be done upstream; the package does not fit models
or calculate posterior probabilities.

## Core quantities

For normalized utilities `U[d, k]` and weights `w[k]`,

```text
EVCA = sum_k w[k] * max_d U[d, k] - max_d sum_k w[k] * U[d, k]
```

The first term is a **disaggregation benchmark**. The second is welfare under
the best single action selected by weighted aggregation. With epistemic
probabilities the gap is EVPI for the model index; with evaluative weights it
is a conditional aggregation shortfall.

`compute_exclusion_costs()` asks a separate question: how much does one model
lose when the decision is selected without that model? Exclusion cost is not
the same as being outvoted in the full aggregation. It depends on the rule used
to reweight the models that remain.

## Sensitivity analysis

`compute_evca_dirichlet()` explores a declared Dirichlet distribution over
weight vectors. For evaluative weights, read this as sensitivity to alternative
standing rules, not as a posterior over which account is correct. It reports
the distribution of the aggregation shortfall and the frequency of each
weighted-optimal decision. The package also provides `plot_dirichlet_evca()`
and `plot_dirichlet_actions()`.

## Related functions

- `normalize_utilities()` applies column-wise min-max scaling. This is a
  normative bridge over the realized action set and can be sensitive to adding
  or removing actions.
- `compute_evpi_gap()` calculates the expected value of perfect information
  for a decision-by-state matrix.
- `weighted_model_utility()` computes model-averaged utility from epistemic
  probabilities; it is distinct from assigning evaluative standing.
- `plot_evca()`, `plot_utilities_heatmap()`, and `plot_exclusion_costs()` show
  the supplied utilities and computed diagnostics.

## Scope

Structural uncertainty and VOI are established decision-analysis topics.
`evca` does not replace EVPI, Bayesian model averaging, or robust decision
methods. Its contribution is a transparent account of the bridge, the selected
aggregation weights, the resulting shortfall, and the consequences of omitting
an account. A common-scale bridge is a substantive choice; the package cannot
establish its legitimacy.
