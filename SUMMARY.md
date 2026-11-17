# evca: package summary

Package `evca`, version 0.5.0. Purpose: decision diagnostics for contested
causal models. The package takes a decision-by-model matrix of expected
utilities that is already on a chosen common scale. It does not build the
causal models, elicit utilities or justify the scale.

## Core quantity

```text
EVCA = sum_k w[k] * max_d U[d, k] - max_d sum_k w[k] * U[d, k]
```

The first term is each model scored at its own best action. The second is the
best single shared action under the weights. How to read the gap depends on
the weights:

- **Epistemic probabilities** (`model_probs`): the gap is the expected value of
  perfect information (EVPI) for the model index.
- **Evaluative weights** (`model_weights`): the gap is an aggregation
  shortfall under declared standing. It is not the value of research.

## Functions

| Group | Functions |
|---|---|
| Core | `compute_evca()`, `compute_evca_dirichlet()`, `evca_decompose()` |
| Scale and agreement | `normalize_utilities()`, `check_action_agreement()` |
| Who loses | `compute_exclusion_costs()` |
| Helpers | `weighted_model_utility()`, `compute_multi_attribute_utilities()`, `compute_evpi_gap()` |
| Examples | `example_evca()`, `example_evca_multidim()`, `example_bavi()` |
| Plots | `plot_evca()`, `plot_utilities_heatmap()`, `plot_sensitivity()`, `plot_exclusion_costs()`, `plot_dirichlet_evca()`, `plot_dirichlet_actions()` |

Notes:

- `check_action_agreement()` uses only each model's own ranking. If all models
  share a best action, EVCA is zero under any positive weights and any monotone
  rescaling. If they do not, it is positive under all of them.
- `compute_evpi_gap()` returns the same number as `compute_evca()`. It names
  the EVPI reading that applies under epistemic probabilities.
- `compute_exclusion_costs()` reports losses on the scale of the matrix it is
  given. Pass the raw matrix with `normalize = FALSE` to see each model's own
  units.

## Vignettes

- `evca-intro`: the framework and basic workflow.
- `irreducible-ambiguity`: the argument that careful framing need not remove
  disagreement between accounts, with a reframing test, bridge sensitivity, an
  acceptability check that needs no common scale, and a joint-venture example.
- `glossary`: terms.

`vignettes/references.bib` was checked against Crossref (see its header).

## Tests

78 tests in `tests/testthat`, all passing on 2026-11-17. Files cover agreement,
all-functions, compute_evca, decompose, dirichlet, exclusion, normalize and
weight interpretation. R CMD check was not run for this summary.

## Known limits

- A common-scale bridge is a substantive choice. Min-max normalization gives
  each model's action range equal stakes and can change when actions are added
  or removed. The size of EVCA depends on the bridge. Only the zero test does
  not.
- No function varies the normalization. The `irreducible-ambiguity` vignette
  compares four scalings by hand. `compute_evca_dirichlet()` varies weights,
  not scalings.
- The value of reframing (original EVCA minus the smallest EVCA across
  independent framings) is described in the vignette and is not a function.
- The package is not a substitute for EVPI, Bayesian model averaging or
  robust decision methods.
