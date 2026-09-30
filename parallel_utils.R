# =============================================================================
# PARALLEL_UTILS.R
# =============================================================================
library(future)
library(furrr)
library(progressr)
library(glue)

`%||%` <- function(x, y) if (is.null(x)) y else x

setup_parallel <- function(enabled = TRUE, workers = NULL) {
  if (!enabled) {
    plan(sequential)
    cat("ℹ Parallel processing disabled — running sequentially\n")
    return(list(enabled = FALSE, workers = 1))
  }
  
  if (is.null(workers)) {
    workers <- max(1, parallel::detectCores() - 1)
  }
  
  # Resolve get() conflict before furrr/future setup
  conflicted::conflicts_prefer(base::get, .quiet = TRUE)
  
  plan(multisession, workers = workers)
  cat(glue("✓ Parallel processing enabled: {workers} workers\n"))
  
  return(list(enabled = TRUE, workers = workers))
}

teardown_parallel <- function() {
  plan(sequential)
  conflicted::conflicts_prefer(base::get, .quiet = TRUE)
  cat("✓ Parallel plan reset to sequential\n")
}

run_models_parallel <- function(model_keys, run_fn, workers = NULL) {
  
  setup_parallel(enabled = TRUE, workers = min(length(model_keys), workers %||% 3))
  # Always reset the future plan, even if a model worker errors.
  on.exit(teardown_parallel(), add = TRUE)
  
  # Enable progress reporting in the console
  progressr::handlers(global = TRUE)
  progressr::handlers("cli") 
  
  results <- progressr::with_progress({
    # Initialize the progress bar with the number of models
    p <- progressr::progressor(steps = length(model_keys))
    
    furrr::future_map(
      model_keys,
      function(model_key) {
        conflicted::conflict_prefer("filter",    "dplyr")
        conflicted::conflict_prefer("select",    "dplyr")
        conflicted::conflict_prefer("AR",        "mvgam")
        conflicted::conflict_prefer("VAR",       "mvgam")
        conflicted::conflict_prefer("RW",        "mvgam")
        conflicted::conflict_prefer("as.matrix", "base")
        
        # Run the model
        res <- run_fn(model_key)
        
        # Tick the progress bar forward and display which model just finished
        p(sprintf("Finished %s", model_key))
        
        return(res)
      },
      .options = furrr::furrr_options(
        seed     = TRUE,
        globals  = TRUE,
        packages = c("dplyr", "tidyr", "glue", "stringr", "conflicted", "mvgam")
      )
    )
  })
  
  names(results) <- model_keys
  return(results)
}