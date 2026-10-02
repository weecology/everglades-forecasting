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
  # "mvgam", "distributional", "verification", 
  # Parallel
  "future", "furrr", "progressr",
  # Config & utils
  "config", "conflicted"
  # (cmdstanr removed from here because it needs a special repo)
)

install.packages(
  cran_packages,
  lib     = lib_path,
  repos   = "https://cloud.r-project.org",
  Ncpus   = 4   
)




# Install spatial packages with explicit lib and repo
# Personal library setup
lib_path <- "~/R/libs"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(lib_path)



# Use RSPM for binaries on HiPerGator (RHEL9)
hpg_repo <- "https://packagemanager.posit.co/cran/__linux__/rhel9/latest"
is_hpg <- dir.exists("/apps")

install.packages(c('sf', 'stars', 'units'), 
                 lib = lib_path, 
                 repos = if(is_hpg) hpg_repo else "https://cloud.r-project.org") [2]




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