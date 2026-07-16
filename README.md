# Wading Bird Population Forecasting

A comparative forecasting framework for Everglades wading bird populations using dynamic GAMs and traditional time series models.

## Overview

This project implements sliding window cross-validation to compare multiple forecasting approaches for six wading bird species in the Florida Everglades. Models incorporate water management covariates and evaluate predictions using both numeric (CRPS) and ordinal (RPS) metrics.

**Species analyzed:**
- Great Blue Heron (gbhe)
- Great Egret (greg)
- Roseate Spoonbill (rosp)
- Snowy Egret (sneg)
- White Ibis (whib)
- Wood Stork (wost)
***spatial hierarchy:***
```r
all
 └── subregion (8 regions: 1, 2a, 2b, 3an, 3as, 3ase, 3b, inlandenp)
      └── colony (33 viable colonies ≥15 years)
``` 

## Installation

```r
# Required packages
install.packages(c(
  "config",        # Configuration management
  "conflicted",    # Namespace conflict handling
  "distributional",# Vectorised probability distributions
  "dplyr",         # Data manipulation
  "fable",         # Traditional time series models
  "feasts",        # Feature extraction
  "ggplot2",       # Figures
  "glue",          # String formatting
  "remotes",       
  "scales", 
  "tidyr",         # Data structure
  "tsibble",       # Time series tibbles
  "verification",  # RPS scoring
  "zoo"            # Interpolation
))

# Install from GitHub
remotes::install_github("nicholasjclark/mvgam")  # Dynamic GAMs
remotes::install_github("weecology/wader")        # Bird count data
remotes::install_github("weecology/edenr")        # Everglades water data
```


## Structure
```
.
├── main.R                          # Main execution script
├── config.yml                      # Configuration settings
├── data_functions.R                # Data loading and preparation
├── evaluation.R                    # Cross-validation and metrics
├── plotting.R                      # Visualization functions
├── final_year_plots.R              # Forecast vs actual for latest year
├── models/
│   ├── mvgam_baseline.R           # Baseline mvgam model
│   ├── mvgam_ar.R                 # AR model with water covariates
│   ├── mvgam_ar_exog.R            # AR with polynomial covariates
│   ├── mvgam_species_specific.R   # Species-specific smooth responses
│   ├── mvgam_trait.R              # Trait-based VAR model
│   └── fable_models.R             # ARIMA, TSLM, ARIMA-exog
└── results/
    ├── run_all_YYYYMMDD-HHMM/
│       ├── forecast_results.rds
│       ├── config.rds
│       ├── mvgam_crps_skill_over_time.png
│       ├── mvgam_rps_skill_over_time.png
│       ├── mvgam_best_model_counts.png
│       └── forecasts/
│           ├── mvgam_gbhe_trait.png
│           ├── mvgam_greg_trait.png
│           └── ... (all species × models)
```     
    
    
## Workflow

# config.yml
choose 
- dgam/fable models you want to use. 
- run fable and dgam
- ordinal evaluation or not
- ordinal breaks
- training and testing windown size
- spatial scale labels:
  - all = Everglades-wide
  - subregion = region
  - colony = colony

```

main.R:
  ↓
if sliding_window_breaks = FALSE:
    Compute breaks once from full dataset
    ↓
    filter_ordinal_years(ordinal_years) → subset if needed
    ↓
    precomputed_breaks = [species-specific quantiles]
else:
    precomputed_breaks = NULL
  ↓
Pass to fit_sliding_window()
  ↓
For each window:
    ↓
    if precomputed_breaks exists:
        Use fixed breaks
    else:
        Compute from training window
        ↓
        filter_ordinal_years(ordinal_years) → subset if needed
```


## Evaluations

# CRPS (numeric)
```
Continuous Ranked Probability Score
↓
Measures: How close is predicted distribution to actual count?
Lower = Better
```
# RPS (Ordinal)
```
Ranked Probability Score
↓
Step 1: Define categories from training data quantiles
        Low: < 33rd percentile
        Medium: 33rd - 50th percentile  
        High: 50th - 67th percentile
        Very High: > 67th percentile
↓
Step 2: Convert predictions to category probabilities
        P(Low) = pnorm(threshold_low, mean=prediction, sd=pred_sd)
        P(Medium) = pnorm(threshold_med, ...) - P(Low)
        ...
↓
Step 3: Score predicted probabilities vs actual category
        Lower = Better
        
#skill score
skill_score = 1 - (model_score / baseline_score)

> 0: Better than baseline
= 0: Same as baseline
< 0: Worse than baseline
```

## Visualization

Four plots generated per model framework:

    - CRPS Skill Over Time - Time series of skill scores by species
    - RPS Skill Over Time - Ordinal accuracy evolution
    - Combined Metrics - All metrics in faceted grid
    - Best Model Counts - Winner frequency by species
    
  
***All settings controlled via config.yml. Key options:***  
```r 
default:
  spatial:
    level: all              # "all", "subregion", or "colony"
    run_by_region: false    # true = separate model per spatial unit
    min_years_required: 10  # Minimum years to include a spatial unit

  models:
    mvgam: [baseline, ar, ar_exog, species_specific, trait]
    fable: [baseline, arima, tslm, arima_exog]

  run_mvgam: true           # Enable/disable mvgam framework
  run_fable: true           # Enable/disable fable framework
  use_ordinal: true         # true = CRPS + RPS, false = CRPS only

  ordinal_breaks: [0.33, 0.50, 0.67]   # Category boundaries (quantiles)
  sliding_window_breaks: true           # true = recompute per window
  ordinal_years: All                    # Years used to define breaks

  train_years: 20           # Training window size
  test_years: 2             # Test window size
  chains: 4                 # MCMC chains
  burnin: 1500              # MCMC warmup iterations
  samples: 1500             # MCMC sampling iterations
```
    