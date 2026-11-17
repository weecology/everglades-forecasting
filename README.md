# Wading Bird Population Forecasting

A comparative forecasting framework for Everglades wading bird populations using dynamic GAMs and traditional time series models.

## Overview
This project implements sliding window cross-validation to compare multiple forecasting approaches for wading bird populations in the Florida Everglades. Models incorporate water management covariates and evaluate predictions using probabilistic numeric accuracy (CRPS), point-forecast accuracy (RMSE), and ordinal accuracy (RPS).

**Species Options:**
The pipeline dynamically handles species filtering based on configuration:
- **`top6` (Default):** 
  - Great Blue Heron (gbhe)
  - Great Egret (greg)
  - Roseate Spoonbill (rosp)
  - Snowy Egret (sneg)
  - White Ibis (whib)
  - Wood Stork (wost)
- **`all`:** All identified species in the database.
- **Custom List:** E.g., `["anhi", "limp", "snki"]`.
- **Unknowns:** Option to retain unidentified counts (e.g., small white heron) via `include_unknowns`.
- **System Totals:** Option to aggregate all selected species into a single `Total` biomass count for system-wide or regional capacity forecasting.

**Spatial Hierarchy:**
```text
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
  "remotes",       # install from github
  "scales",        # for nice plotts 
  "tidyr",         # Data structure
  "tsibble",       # Time series tibbles
  "verification",  # RPS scoring
  "zoo",           # Interpolation
  "future",        # Parallel processing
  "furrr",         # Parallel mapping
  "digest"         # Caching
  "pdftools"       # Save all forecasts to pdf
))

# Install from GitHub
remotes::install_github("nicholasjclark/mvgam")  # Dynamic GAMs
remotes::install_github("weecology/wader")        # Bird count data
remotes::install_github("weecology/edenr")        # Everglades water data
remotes::install_github("hendersontrent/fable.gam") #Fable GAMs
```


## Strucutre
```
.
├── main.R                          # Main execution script
├── config.yml                      # Configuration profiles & settings
├── data_functions.R                # Data loading, aggregation, and caching
├── evaluation.R                    # Cross-validation and metrics
├── plotting.R                      # Visualization functions
├── generate_forecast_pdfs.R        # Forecast PDF generation and .rds cleanup
├── cache/                          # Auto-generated data/model cache folder
├── models/
│   ├── mvgam_baseline.R            # Baseline longterm average model
│   ├── mvgam_ar.R                  # AR model with smooth water covariates
│   ├── mvgam_ar_exog.R             # AR with linear covariates
│   ├── mvgam_ar_exog_plus.R        # AR with tensor product interactions
│   ├── mvgam_species_specific.R    # Species-specific smooth responses
│   ├── mvgam_trait.R               # Trait-weighted covariate effects
│   ├── mvgam_trait2.R              # Traits + guilds + body size
│   └── fable_models.R              # ARIMA, TSLM, ARIMA-exog, GAM
└── results/
    └── <run_folder>/
        ├── forecast_results.rds
        ├── metrics_only.rds
        ├── config.rds
        ├── forecast_pdf_manifest.csv
        ├── mvgam_crps_skill_over_time.png
        ├── mvgam_rps_skill_over_time.png
        ├── mvgam_best_model_counts.png
        └── windows/
            └── <location>_<test_year>/
                ├── train_data.rds
                ├── test_data.rds
                └── mvgam/
                    └── <model>/
                        ├── forecast.pdf   # Native forecast plots
                        └── metrics.rds
```     
 ### Forecast horizon

In the current `main.R`, an explicit `forecast_horizon` overrides `test_years`.

Both frameworks forecast the complete held-out test window. For annual data, a horizon of `2` means two forecast years per CV window.

These are cross-validation forecasts, not additional forecasts beyond the latest observation year.

The wrapper can override the horizon:

```r
FORECAST_HORIZON <- 2
base_config$forecast_horizon <- FORECAST_HORIZON
```   
    
## Workflow

#config.R
choose 
- dgam/fable models you want to use. 
- run fable and dgam
- ordinal evaluation or not
- ordinal breaks
- training and testing windown size

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
    Save train_data.rds and test_data.rds to disk
    ↓
    For each model:
        Fit model → save forecast.rds → generate native PDF → delete .rds
    ↓
    if precomputed_breaks exists:
        Use fixed breaks
    else:
        Compute from training window
        ↓
        filter_ordinal_years(ordinal_years) → subset if needed
  ↓
Save metrics_only.rds (small, for cross-scale comparisons)
  ↓
generate_plots() → skill plots + forecast PDFs
```


## Evaluations

# CRPS (numeric)
```
Continuous Ranked Probability Score
↓
Measures: How close is predicted distribution to actual count?
Lower = Better
```

### RMSE (numeric point-forecast accuracy)
Root Mean Squared Error measures the difference between point forecasts and observed counts:

```text
RMSE = sqrt(mean((prediction - observed_count)^2))
```

RMSE is expressed in the same units as the observed counts and penalizes large errors more strongly than small errors.

**Lower = Better.**

The pipeline calculates RMSE within each cross-validation test window, separately for each model and species, and each region where applicable.

Point forecasts differ between frameworks:
- **mvgam:** Uses the predictive median (`Estimate`, extracted from `predQ50`).
- **fable:** Uses the predictive mean (`.mean`).

This distinction should be considered when comparing RMSE across frameworks. For a one-year test window with one observation per series, RMSE equals the absolute forecast error.

RMSE is calculated independently of ordinal evaluation: disabling `use_ordinal` does not disable RMSE.



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
```

# skill score
```
crps_skill = 1 - (model_crps / baseline_crps)
rmse_skill = 1 - (model_rmse / baseline_rmse)
rps_skill  = 1 - (model_rps / baseline_rps)


> 0: Better than baseline
= 0: Same as baseline
< 0: Worse than baseline
```

A successful baseline forecast is required to calculate skill scores.
Missing baseline scores or baseline scores of zero produce `NA`
because the ratio cannot be calculated.


## Visualization

Four plots generated per model framework:

    - CRPS Skill Over Time - Time series of skill scores by species
    - RPS Skill Over Time - Ordinal accuracy evolution
    - Combined Metrics - All metrics in faceted grid
    - Best Model Counts - Winner frequency by species
    
 
The `run_all_scales.R` wrapper includes available CRPS, RMSE, and RPS
skill scores in:
- Per-model window skill plots by spatial scale.
- Per-model and cross-model window delta plots.
- Marginal distribution plots.
- Density, ECDF, and violin plots of skill deltas.
- Per-window CSV outputs and aggregated summaries.

RMSE appears under the **RMSE Skill** label.
```text
delta_skill = system_skill - subregion_skill

Positive: System has higher skill relative to its baseline.
Negative: Subregion has higher skill relative to its baseline.
```

These deltas compare baseline-relative skill, not raw RMSE values.

***All settings controlled via config.yml. Key options:***  
```r 
default:
  spatial:
    level: all                    # "all", "subregion", or "colony"
    run_by_region: false          # true = separate model per spatial unit
    forecast_totals: false        # true = sum counts to a single 'Total' category
    include_species: "top6"       # "top6", "all", or list e.g., ["wost", "whib"]
    include_unknowns: true        # true = keep 'unkn', 'smwh', etc. when using 'all'
    min_years_required: 10        # Minimum years to include a spatial unit
    
  models:
    mvgam: [baseline, ar, ar_exog, ar_exog_plus, species_specific, trait, trait2]
    fable: [baseline, arima, tslm, arima_exog, gam]
    
  run_mvgam: true                 # Enable/disable mvgam framework
  run_fable: true                 # Enable/disable fable framework
  
  use_ordinal: true               # true = CRPS + RPS, false = CRPS only
  ordinal_breaks: [0.33, 0.50, 0.67] # Category boundaries (quantiles)
  sliding_window_breaks: true     # true = recompute per window
  ordinal_years: All              # Years used to define breaks
  
  train_years: 20                 # Training window size
  test_years: 2                   # Test window size
  
  parallel:
    enabled: true                 # Multicore processing
    workers: null                 # null = auto-detect cores
    
  cache:
    data: true                    # Speed up reruns by caching data prep
    models: false                 # Speed up reruns by caching fitted models
    
  chains: 4                       # MCMC chains
  burnin: 1500                    # MCMC warmup iterations
  samples: 1500                   # MCMC sampling iterations
```
    
  ### Testing one window

The pipeline does not retain every window's posterior draws in memory.

```text
Load configuration and prepared data
    ↓
Select one CV window
    ↓
Save train_data.rds and test_data.rds
    ↓
For each selected model:
    Fit model
    Generate held-out forecast
    Save forecast.rds
    Optionally generate native PNG plots
    Extract small predictions and metrics
    Release the raw forecast object
    ↓
Proceed to the next window
    ↓
Save lightweight combined results and metrics_only.rds
    ↓
Run cross-scale comparisons
    ↓
Build one combined forecast PDF
    ↓
Validate PDF pages and labels
    ↓
Optionally delete processed raw forecasts and window inputs
```

Each model function retains its existing return format:

```r
list(fc = fc, crps = crps)
```

The returned `window_objects` records contain metadata and relative file paths rather than full posterior arrays.

Saving to disk reduces accumulation across windows, but an individual Stan fit and prediction calculation can still require substantial memory.

### Window directory structure

Before cleanup:

```text
<model>/<scale>/windows/
└── <location>_<first_test_year>/
    ├── train_data.rds
    ├── test_data.rds
    └── mvgam/
        ├── metadata.rds
        ├── predictions.rds
        ├── metrics.rds
        ├── baseline/
        │   ├── forecast.rds
        │   └── plots/
        └── ar_exog/
            ├── forecast.rds
            └── plots/
```

For example, `1_2024` identifies spatial unit `1` and a forecast window beginning in 2024.

Fable forecasts are saved under the window's `fable/` directory. Their saved layout can differ because one fable object may contain multiple models and series.  

## Combined Forecast PDF

The wrapper's final PDF stage creates:

```text
<scale_run_folder>/all_models_forecasts.pdf
```

The PDF is saved alongside the cross-model and cross-scale comparison figures.

It contains one page per saved model/series forecast, covering the available models, species, regions, spatial scales, and windows.

Pages are labelled with:

```text
Scale | Window | Model | Series
```  

### Fable pages

## Verified Cleanup

The final PDF section contains:

```r
DELETE_RAW_AFTER_VALIDATION <- TRUE
```

Set it to `FALSE` for the first test to retain source data while inspecting the combined PDF.

When enabled, cleanup deletes only:

- The processed `forecast.rds` files
- Their associated `train_data.rds` files
- Their associated `test_data.rds` files

Cleanup retains:

- `forecast_results.rds`
- `metrics_only.rds`
- Configuration files
- Window metadata
- Small prediction and metric tables
- Comparison figures and CSV summaries
- The combined PDF and manifests
- Existing PNG plots

**This is not a blanket deletion of all `.rds` files or every large file.**

Deletion results and estimated reclaimed space are recorded in:

```text
forecast_cleanup_manifest.csv
```

After cleanup, raw forecast paths in the saved metadata refer to removed files. The PDF preserves figures, not predictive draws or fitted models.

Consequently, the removed forecast distributions cannot be rescored or replotted from the PDF alone. Retain a separate backup if further posterior analysis is needed.

## Final Results Structure

```text
results/
└── top6_model_scale_run_<models>_<scales>-<timestamp>/
    ├── all_models_forecasts.pdf
    ├── forecast_pdf_manifest.csv
    ├── forecast_cleanup_manifest.csv
    ├── base_config.rds
    ├── window_skill_all_scales.csv
    ├── all_models_window_delta.png
    ├── all_models_window_delta.csv
    ├── all_models_summary.csv
    ├── all_models_delta_density.png
    ├── all_models_delta_ecdf.png
    ├── all_models_delta_violin.png
    ├── window_delta_marginal/
    ├── mvgam_ar/
    │   ├── window_skill_by_scale.png
    │   ├── window_delta_skill.png
    │   ├── window_delta_skill.csv
    │   ├── system/
    │   └── subregion/
    └── mvgam_ar_exog/
        ├── system/
        └── subregion/
```

## Running on HiPerGator

Install packages on a **login node** and run models on a **compute node**. Both use the shared R package library under `/blue`.

### 1. Set up R and install packages on the login node

From the project directory:

    source setup_env.sh
    Rscript install_packages.R

`setup_env.sh` loads R/4.6 and sets the package library to `/blue/ewhite/alexanderblochel/R/libs`. Do not load a separate GDAL module; on this system, it replaces the R module.


### 2. Request a compute session

    srundev --time 2:00:00 --mem 32gb
    

### 3. Run the pipeline on the compute node

From the project directory:

    source setup_env.sh
    Rscript run_all_scales.R

You do not need to reinstall packages on the compute node. If a package is missing, install it on the login node into the shared `/blue` library, then retry. If an external download fails on the compute node, download or cache that data from the login node first.

### 4. Retrieve results

On HiPerGator, the final block of `run_all_scales.R` creates a `.zip` archive of `scale_run_folder` for Globus transfer and removes the original results folder. Check that the archive exists and contains the expected files. This archiving step is skipped when running locally.

### Retrieve results

The current wrapper creates a ZIP archive on HiPerGator and removes the original scale-run directory afterward.

Before relying on that removal step, verify that archive creation succeeded and that the archive contains the combined PDF, manifests, configurations, and comparison outputs. A non-empty ZIP alone does not establish completeness.

## Troubleshooting

### Empty forecast PDFs

Explicitly print returned ggplot/patchwork objects inside loops. Do not delete raw forecasts based only on a PDF's file size.

Use the combined PDF's page-count and label validation.

### Missing raw forecasts

Older individual-PDF cleanup may already have removed `forecast.rds` files. An empty PDF cannot reconstruct the original predictive draws.

### Out-of-memory termination

Keep posterior forecasts on disk and load one model at a time. Avoid accumulating full forecast objects across windows.

Use separate metrics files during post-processing, keep outer parallelism modest, and allocate enough memory for the largest individual fit.

### Extreme forecast intervals

Successful sampling does not guarantee convergence or reliable predictions. Inspect sampling diagnostics, predictive tails, covariate extrapolation, and the adequacy of the training window.

Do not treat `type = "response"` as a general solution to extreme forecast values.

### Package-loading failures

Install or repair packages using the same R version and shared library used by the batch job.

### Data updates and prepared caches

Refreshing source observations or water data must also invalidate affected prepared-data caches. Otherwise, the pipeline can continue using an older cached dataset.
