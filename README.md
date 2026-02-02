# Expected Value of Eliminating Causal Ambiguity: Decision-Theoretic Analysis

## Overview

This project implements a framework for calculating the **Expected Value of Eliminating Causal Ambiguity (EVECA)** when multiple competing causal models exist for a decision problem. It extends traditional Value of Information (VOI) methods to handle structural uncertainty about causal mechanisms, providing decision-theoretic tools for valuing causal clarity in complex decision contexts.

## Background

Causal ambiguity arises when decision-makers face multiple competing causal models of a phenomenon, each suggesting different intervention effects and policy implications. Unlike parameter uncertainty within a fixed causal structure, causal ambiguity involves uncertainty about which causal graph or structural model is correct.

Traditional Expected Value of Perfect Information (EVPI) and Expected Value of Partial Perfect Information (EVPPI) methods assume fixed causal structures and only address parameter uncertainty. This project develops methods to quantify the value of resolving structural (causal-model) ambiguity using Bayesian model averaging and decision-theoretic frameworks.

## Project Structure

```
26_ev_ambiguity/
├── README.md                           # This file
├── config.yaml                         # Project configuration and metadata
├── report.Rmd                          # Main analysis report (R Markdown)
├── bib/
│   └── references.bib                  # Bibliography with real citations
└── cases/                              # Application case studies
    ├── health_economics/               # Health technology assessment examples
    ├── environmental_policy/           # Climate and environmental decisions
    └── business_strategy/              # Strategic management and investment
```

### Key Files

- **`config.yaml`**: Project configuration including metadata, analysis settings, dependencies, and file paths.
- **`report.Rmd`**: Main analysis report with theoretical framework, methodology, computational approaches, and integrated case study results.
- **`bib/references.bib`**: BibTeX bibliography containing real citations from the literature on value of information, causal ambiguity, Bayesian model averaging, and decision analysis.

### Case Studies

The project includes three application domains, each with detailed case studies:

1. **Health Economics**: Treatment selection, screening policies, and vaccine allocation under causal ambiguity
2. **Environmental Policy**: Carbon pricing, renewable energy investment, and ecosystem management under structural uncertainty
3. **Business Strategy**: Market entry, R&D portfolio allocation, and mergers & acquisitions under competitive ambiguity

Each case study directory contains analysis notebooks, model specifications, data descriptions, and implementation details.

## Installation and Dependencies

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (recommended) or another R IDE
- **Pandoc** (for rendering R Markdown documents)

### R Packages

Install required packages with:

```r
install.packages(c(
  "tidyverse",
  "ggplot2",
  "rmarkdown",
  "knitr",
  "kableExtra",
  "bookdown",
  "scales"
))
```

## Usage

### Rendering the Main Report

To generate the complete analysis report:

```r
# In R, from the project root directory:
rmarkdown::render("report.Rmd", output_format = "html_document")
# or for PDF:
rmarkdown::render("report.Rmd", output_format = "pdf_document")
```

### Running Case Studies

Navigate to individual case study directories and run the analysis notebooks:

```r
setwd("cases/health_economics/01_anticoagulant/")
rmarkdown::render("analysis.Rmd")
```

### Customizing Analysis

Edit `config.yaml` to modify analysis settings, or create new case studies by following the template structure in the existing directories.

## Theoretical Framework

The project implements a Bayesian model averaging approach for decision-making under causal ambiguity:

1. **Model Specification**: Define competing causal models as directed acyclic graphs (DAGs) or structural equations
2. **Prior Elicitation**: Assign prior probabilities to models based on expert knowledge and existing evidence
3. **Posterior Computation**: Update model probabilities using available data
4. **Expected Utility Calculation**: Compute expected outcomes for each decision alternative under each model
5. **EVECA Estimation**: Calculate the expected value of eliminating causal ambiguity:

   \[
   \text{EVECA} = \max_d EU(d|M^*) - \max_d \sum_{k=1}^K p(M_k|D) \cdot EU(d|M_k)
   \]

   where \(EU_{BMA}(d) = \sum_{k=1}^K p(M_k|D) \cdot EU(d|M_k)\) is the Bayesian model average expected utility.

## Key Features

- **Decision-theoretic foundation** for valuing causal clarity
- **Bayesian model averaging** implementation for structural uncertainty
- **Computational methods** adapted from EVPPI literature
- **Multiple application domains** with real-world relevance
- **Reproducible analysis** using R Markdown and version control
- **Extensible framework** for additional case studies and methods

## References

The project builds on work from multiple disciplines:

- **Value of Information**: @coyle2008estimating, @heath2017review, @rothery2020value
- **Causal Ambiguity**: @mosakowski1997strategy, @king2007disentangling
- **Bayesian Model Averaging**: @kaptein2025incorporating, @papamichalis2021bayesian
- **Computational Methods**: @strong2013estimating, @fang2021multilevel, @brennan2007calculating
- **Behavioral Aspects**: @alnajjar2025systems, @jirousek2020subjective, @mastrogiorgio2024quantum

Complete citations are available in `bib/references.bib`.

## Contributing

To contribute to this project:

1. Fork the repository
2. Create a feature branch
3. Add new case studies following the existing structure
4. Document methods and results thoroughly
5. Submit a pull request with clear description of changes

New case studies should:
- Address a genuine decision problem with causal ambiguity
- Include multiple competing causal models
- Implement the standard EVECA analysis framework
- Provide reproducible code and data descriptions

## License

This project is available under the MIT License. See the LICENSE file for details.

## Acknowledgments

This work builds on decades of research in decision analysis, causal inference, Bayesian statistics, and domain-specific applications. Special thanks to the researchers developing value of information methods and causal decision theory, whose work makes such integrative frameworks possible.

## Contact

For questions, suggestions, or collaborations, please open an issue in the project repository or contact the maintainers.