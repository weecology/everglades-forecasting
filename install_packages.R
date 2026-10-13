# =============================================================================
# install_packages.R
# Install project R packages into the personal HiPerGator library on /blue.
#
# Before running:
#   1. Load the R and spatial-library modules used by the project.
#   2. Run with that same R version/environment when running the pipeline.
#
# Run:
#   Rscript install_packages.R
# =============================================================================

lib_path <- "/blue/ewhite/alexanderblochel/R/libs"

if (!dir.exists("/blue")) {
  stop("This installer is intended for HiPerGator: /blue was not found.")
}

if (!dir.create(lib_path, recursive = TRUE, showWarnings = FALSE) &&
    !dir.exists(lib_path)) {
  stop("Could not create R library directory: ", lib_path)
}

if (file.access(lib_path, 2) != 0) {
  stop("R library directory is not writable: ", lib_path)
}

.libPaths(c(lib_path, .libPaths()))

cat("R version: ", R.version.string, "\n", sep = "")
cat("Library:   ", normalizePath(lib_path), "\n", sep = "")
cat("Library paths:\n")
print(.libPaths())

# Use CRAN source packages. This avoids selecting binaries for an OS that may
# not match the HiPerGator compute environment. Source installation requires
# available compilers and compatible system libraries.
cran_repo <- "https://cloud.r-project.org"
options(repos = c(CRAN = cran_repo))

cran_packages <- c(
  # Core data
  "digest", "dplyr", "tidyr", "readr", "tsibble", "zoo",
  
  # Visualization
  "ggplot2", "patchwork", "stringr", "glue",
<<<<<<< HEAD
  
  # Modeling
  "mvgam", "distributional", "verification",
  
=======
  # Modelling
  # "mvgam", "distributional", "verification", 
>>>>>>> weecology/main
  # Parallel
  "future", "furrr", "progressr",
  
  # Configuration and utilities
  "config", "conflicted", "remotes",
  
  # Spatial
  "sf", "stars", "units"
)

cat("\nInstalling CRAN packages into ", lib_path, "...\n", sep = "")
install.packages(
  cran_packages,
  lib = lib_path,
  repos = cran_repo,
  Ncpus = 1
)

# cmdstanr is distributed through the Stan r-universe, with CRAN as a
# fallback for dependencies. This does not install CmdStan itself.
cat("\nInstalling cmdstanr...\n")
install.packages(
  "cmdstanr",
  lib = lib_path,
  repos = c(
    "https://stan-dev.r-universe.dev",
    CRAN = cran_repo
  ),
  Ncpus = 1
)

# These GitHub packages are installed from source.
cat("\nInstalling edenR from GitHub...\n")
remotes::install_github(
  "weecology/edenR",
  lib = lib_path,
  dependencies = TRUE,
  upgrade = "never"
)

cat("\nInstalling wader from GitHub...\n")
remotes::install_github(
  "weecology/wader",
  lib = lib_path,
  dependencies = TRUE,
  upgrade = "never"
)

<<<<<<< HEAD
# Confirm installation and test that the key packages can be loaded.
requested_packages <- c(cran_packages, "cmdstanr", "edenR", "wader")
installed <- rownames(installed.packages(lib.loc = lib_path))
missing <- setdiff(requested_packages, installed)

if (length(missing) > 0) {
  cat("\nPackages missing from ", lib_path, ":\n", sep = "")
  cat(paste0("  - ", missing, collapse = "\n"), "\n")
  stop("Some requested packages were not installed.")
}

cat("\nTesting package loads...\n")
for (pkg in requested_packages) {
  cat("  ", pkg, ": ", sep = "")
  loadNamespace(pkg, lib.loc = lib_path)
  cat("OK\n")
}

cat("\nAll requested packages are installed and load in:\n", lib_path, "\n", sep = "")
cat("Note: cmdstanr is installed, but CmdStan itself must be installed separately.\n")
=======
# Install spatial packages with explicit lib and repo
# Personal library setup
lib_path <- "~/R/libs"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(lib_path)



# Use RSPM for binaries on HiPerGator (RHEL9)
hpg_repo <- "https://packagemanager.posit.co/cran/__linux__/rhel9/latest"
is_hpg <- dir.exists("/apps")

# Install units first with explicit udunits2 paths
install.packages('units',
                 lib = lib_path,
                 repos = if(is_hpg) hpg_repo else "https://cloud.r-project.org",
                 configure.args = '--with-udunits2-lib=/apps/udunits/2.2.17/lib --with-udunits2-include=/apps/udunits/2.2.17/include',
                 configure.vars = 'CFLAGS=-fexceptions CXXFLAGS=-fexceptions')

# Then sf and stars
install.packages(c('sf', 'stars'),
                 lib = lib_path,
                 repos = if(is_hpg) hpg_repo else "https://cloud.r-project.org")


# Install cmdstanr from its custom repository
install.packages("cmdstanr", lib = lib_path, repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org"))

# =============================================================================
# GITHUB PACKAGES
# =============================================================================
install.packages("remotes", lib = lib_path, repos = "https://cloud.r-project.org")
remotes::install_github("weecology/edenR", lib = lib_path, upgrade = "never")
remotes::install_github("weecology/wader",  lib = lib_path, upgrade = "never")

# =============================================================================
# CMDSTAN
# =============================================================================
library(cmdstanr)
cmdstanr::install_cmdstan(cores = 8)

# =============================================================================
# VERIFY
# =============================================================================
pkgs <- c(cran_packages, "sf", "stars", "units", "cmdstanr", "edenR", "wader")
missing <- pkgs[!pkgs %in% installed.packages(lib.loc = lib_path)[, "Package"]]
if (length(missing) == 0) {
  cat("✓ All packages installed successfully\n")
} else {
  cat("✗ Missing packages:\n")
  cat(paste(" •", missing, collapse = "\n"), "\n")
}
>>>>>>> weecology/main
