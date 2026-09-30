# =============================================================================
# install_packages.R
# =============================================================================

# Personal library
lib_path <- "~/R/libs"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(lib_path)

# =============================================================================
# CRAN PACKAGES
# =============================================================================
cran_packages <- c(
  # Core data
  "digest", "dplyr", "tidyr", "readr", "tsibble", "zoo",
  # Visualisation
  "ggplot2", "patchwork", "stringr", "glue",
  # Modelling
  "mvgam", "distributional", "verification",
  # Parallel
  "future", "furrr", "progressr",
  # Config & utils
  "config", "conflicted",
  # Stan interface
  "cmdstanr"
)

install.packages(
  cran_packages,
  lib     = lib_path,
  repos   = "https://cloud.r-project.org",
  Ncpus   = 4   # parallelise the build
)

# =============================================================================
# GITHUB PACKAGES
# =============================================================================
install.packages("remotes", lib = lib_path, repos = "https://cloud.r-project.org")

remotes::install_github("weecology/edenR", lib = lib_path, upgrade = "never")
remotes::install_github("weecology/wader",  lib = lib_path, upgrade = "never")

# =============================================================================
# CMDSTAN
# =============================================================================
cmdstanr::install_cmdstan(cores = 8)

# =============================================================================
# VERIFY
# =============================================================================
pkgs <- c(cran_packages, "edenR", "wader")
missing <- pkgs[!pkgs %in% installed.packages(lib.loc = lib_path)[, "Package"]]
if (length(missing) == 0) {
  cat("✓ All packages installed successfully\n")
} else {
  cat("✗ Missing packages:\n")
  cat(paste(" •", missing, collapse = "\n"), "\n")
}