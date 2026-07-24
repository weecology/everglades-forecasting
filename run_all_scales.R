# =============================================================================
# WRAPPER SCRIPT: RUN ALL SPATIAL SCALES AND COMPARE
# =============================================================================
library(config)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

# =============================================================================
# CREATE TIMESTAMPED SCALE_RUN FOLDER
# =============================================================================

timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
scale_run_folder <- file.path("results", paste0("scale_run_", timestamp))

# Create main folder and subfolders for each scale
dir.create(scale_run_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scale_run_folder, "system"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scale_run_folder, "subregion"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(scale_run_folder, "colony"), recursive = TRUE, showWarnings = FALSE)

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SPATIAL SCALE COMPARISON RUN\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("📂 Results folder:", scale_run_folder, "\n\n")

# =============================================================================
# LOAD BASE CONFIGURATION
# =============================================================================

base_profile <- "run_all_scales_all"
Sys.setenv(R_CONFIG_ACTIVE = base_profile)
base_config <- config::get()

# Save the base configuration
saveRDS(base_config, file.path(scale_run_folder, "base_config.rds"))
cat("✓ Base configuration saved\n\n")

# =============================================================================
# SCALES TO RUN
# =============================================================================

scales_to_run <- c("system", "subregion", "colony")
completed_folders <- list()

# =============================================================================
# PART 1: RUN THE PIPELINE FOR EACH SCALE
# =============================================================================

for (current_scale in scales_to_run) {
  
  cat("\n")
  cat(paste(rep("█", 80), collapse = ""), "\n")
  cat("🚀 STARTING PIPELINE FOR SCALE:", toupper(current_scale), "\n")
  cat(paste(rep("█", 80), collapse = ""), "\n\n")
  
  # Create CONFIG for this scale
  CONFIG <- base_config
  CONFIG$spatial$level <- current_scale
  
  # For system-wide ("system"), ensure run_by_region is FALSE
  if (current_scale == "system") {
    CONFIG$spatial$run_by_region <- FALSE
  }
  
  # Override run_folder in main.R to save to our scale_run subfolder
  # We'll do this by temporarily modifying the results path
  original_results_dir <- "results"
  
  # Run the pipeline with error handling
  tryCatch({
    # Temporarily change working concept - main.R will create run_folder
    # We need to capture it and move results
    source("main.R")
    
    # Move the results from the default location to our scale_run subfolder
    # main.R creates: results/run_{level}_{timestamp}/
    # We want: results/scale_run_{timestamp}/{level}/
    
    if (exists("run_folder") && !is.null(run_folder)) {
      # Copy all contents to our organized structure
      dest_folder <- file.path(scale_run_folder, current_scale)
      
      # Copy all files from run_folder to dest_folder
      files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
      
      for (src_file in files_to_copy) {
        rel_path <- sub(paste0(run_folder, "/"), "", src_file)
        dest_file <- file.path(dest_folder, rel_path)
        dest_dir <- dirname(dest_file)
        
        dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        file.copy(src_file, dest_file, overwrite = TRUE)
      }
      
      # Store the new location
      completed_folders[[current_scale]] <- dest_folder
      
      # Clean up original run_folder
      unlink(run_folder, recursive = TRUE)
      
      cat("✓ Results moved to:", dest_folder, "\n")
    } else {
      warning(glue::glue("Scale {current_scale}: run_folder not created"))
      completed_folders[[current_scale]] <- NULL
    }
    
  }, error = function(e) {
    warning(glue::glue("Scale {current_scale} failed: {e$message}"))
    completed_folders[[current_scale]] <- NULL
  })
  
  # Clean up memory before the next run
  gc()
}

cat("\n✅ ALL FORECASTING RUNS COMPLETE!\n")
cat("Folders generated:\n")
print(completed_folders)

# =============================================================================
# PART 2: EXTRACT AND COMBINE METRICS (CRPS, RPS, RMSE)
# =============================================================================

cat("\n📊 Extracting metrics from all scales...\n")

# Enhanced extraction function - gets CRPS, RPS, and RMSE
extract_metrics <- function(folder_path, scale_name) {
  file_path <- file.path(folder_path, "forecast_results.rds")
  if (!file.exists(file_path)) {
    cat("  ⚠️  No results file for", scale_name, "\n")
    return(NULL)
  }
  
  res <- readRDS(file_path)
  combined_metrics <- tibble()
  
  # Extract mvgam metrics
  if (!is.null(res$mvgam) && !is.null(res$mvgam$metrics) && nrow(res$mvgam$metrics) > 0) {
    m_mvgam <- res$mvgam$metrics |>
      filter(model != "baseline") |>
      select(model, species, 
             crps_skill, 
             any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(framework = "mvgam")
    combined_metrics <- bind_rows(combined_metrics, m_mvgam)
  }
  
  # Extract fable metrics
  if (!is.null(res$fable) && !is.null(res$fable$metrics) && nrow(res$fable$metrics) > 0) {
    m_fable <- res$fable$metrics |>
      filter(.model != "baseline") |>
      rename(model = .model) |>
      select(model, species, 
             crps_skill, 
             any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(framework = "fable")
    combined_metrics <- bind_rows(combined_metrics, m_fable)
  }
  
  if (nrow(combined_metrics) == 0) return(NULL)
  
  combined_metrics <- combined_metrics |> mutate(scale = scale_name)
  return(combined_metrics)
}

# Extract metrics from all scales
data_system       <- extract_metrics(completed_folders[["system"]], "system")
data_subregion <- extract_metrics(completed_folders[["subregion"]], "Region")
data_colony    <- extract_metrics(completed_folders[["colony"]], "Colony")

# Combine all metrics
plot_data <- bind_rows(data_colony, data_subregion, data_all)

if (nrow(plot_data) == 0) {
  stop("No metric data found across any scale. Did the models fail to fit?")
}

# Add scale factor and winsorize
plot_data <- plot_data |>
  mutate(
    scale = factor(scale, levels = c("Colony", "Region", "System"))
  )

# Save combined metrics
saveRDS(plot_data, file.path(scale_run_folder, "combined_metrics.rds"))
cat("✓ Combined metrics saved\n")

cat("\nMetrics summary:\n")
cat("  Total data points:", nrow(plot_data), "\n")
cat("  Scales:", paste(unique(plot_data$scale), collapse = ", "), "\n")
cat("  Metrics available:", paste(names(plot_data)[grepl("skill", names(plot_data))], collapse = ", "), "\n")

# =============================================================================
# PART 3: GENERATE COMPARISON PLOTS FOR EACH METRIC
# =============================================================================

cat("\n📈 Generating comparison plots...\n")

# Define colors
scale_colors <- c("Colony" = "#00B050", "Region" = "#FF0000", "system" = "#000000")

# Available skill metrics
skill_metrics <- intersect(c("crps_skill", "rps_skill", "rmse_skill"), names(plot_data))

# Create long format for faceted plotting
plot_data_long <- plot_data |>
  select(model, species, framework, scale, all_of(skill_metrics)) |>
  pivot_longer(cols = all_of(skill_metrics), 
               names_to = "metric", 
               values_to = "skill_score") |>
  filter(!is.na(skill_score)) |>
  mutate(
    skill_score = ifelse(skill_score < -1, -1, skill_score),  # Winsorize at -1
    metric_label = case_when(
      metric == "crps_skill" ~ "CRPS Skill",
      metric == "rps_skill" ~ "RPS Skill",
      metric == "rmse_skill" ~ "RMSE Skill",
      TRUE ~ metric
    )
  )

cat("  Winsorized values (< -1):", sum(plot_data_long$skill_score == -1), "\n")

# -----------------------------------------------------------------------------
# PLOT 1: Combined density plot (all metrics faceted)
# -----------------------------------------------------------------------------

p_density_combined <- ggplot(plot_data_long, 
                             aes(x = skill_score, fill = scale, color = scale)) +
  geom_density(alpha = 0.2, linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = scale_colors) +
  scale_color_manual(values = scale_colors) +
  theme_classic(base_size = 14) +
  labs(
    title = "Forecast Skill Across Spatial Scales",
    x = "Skill Score",
    y = "Density",
    fill = "Scale",
    color = "Scale"
  ) +
  theme(
    legend.position = "bottom",
    axis.line = element_line(linewidth = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(file.path(scale_run_folder, "combined_density_all_metrics.png"), 
       p_density_combined, width = 10, height = 12, dpi = 300)
cat("  ✓ Combined density plot saved\n")

# -----------------------------------------------------------------------------
# PLOT 2: Combined jitter plot (all metrics)
# -----------------------------------------------------------------------------

p_jitter_combined <- ggplot(plot_data_long, 
                            aes(x = scale, y = skill_score, color = scale)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  facet_wrap(~metric_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = scale_colors) +
  theme_classic(base_size = 14) +
  labs(
    title = "Forecast Skill by Spatial Scale and Metric",
    x = NULL,
    y = "Skill Score",
    color = "Scale"
  ) +
  theme(
    legend.position = "bottom",
    axis.line = element_line(linewidth = 1),
    axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(file.path(scale_run_folder, "combined_jitter_all_metrics.png"), 
       p_jitter_combined, width = 12, height = 8, dpi = 300)
cat("  ✓ Combined jitter plot saved\n")

# -----------------------------------------------------------------------------
# PLOT 3: Individual plots for each metric
# -----------------------------------------------------------------------------

for (metric in skill_metrics) {
  
  metric_label <- case_when(
    metric == "crps_skill" ~ "CRPS",
    metric == "rps_skill" ~ "RPS",
    metric == "rmse_skill" ~ "RMSE",
    TRUE ~ metric
  )
  
  metric_data <- plot_data_long |> filter(metric == !!metric)
  
  if (nrow(metric_data) == 0) next
  
  # Density plot
  p_dens <- ggplot(metric_data, aes(x = skill_score, fill = scale, color = scale)) +
    geom_density(alpha = 0.2, linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    scale_fill_manual(values = scale_colors) +
    scale_color_manual(values = scale_colors) +
    theme_classic(base_size = 14) +
    labs(
      title = paste(metric_label, "Skill Score Across Spatial Scales"),
      x = "Skill Score",
      y = "Density",
      fill = "Scale",
      color = "Scale"
    ) +
    theme(
      legend.position = "bottom",
      axis.line = element_line(linewidth = 1)
    )
  
  ggsave(file.path(scale_run_folder, paste0(metric, "_density.png")), 
         p_dens, width = 10, height = 6, dpi = 300)
  
  # Jitter plot
  p_jit <- ggplot(metric_data, aes(x = scale, y = skill_score, color = scale)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.8) +
    geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
    scale_color_manual(values = scale_colors) +
    theme_classic(base_size = 14) +
    labs(
      title = paste(metric_label, "Skill Score by Spatial Scale"),
      x = NULL,
      y = "Skill Score"
    ) +
    theme(
      legend.position = "none",
      axis.line = element_line(linewidth = 1),
      axis.text.x = element_text(face = "bold", size = 14)
    )
  
  ggsave(file.path(scale_run_folder, paste0(metric, "_jitter.png")), 
         p_jit, width = 8, height = 6, dpi = 300)
  
  cat("  ✓", metric_label, "plots saved\n")
}

# -----------------------------------------------------------------------------
# PLOT 4: Summary statistics table
# -----------------------------------------------------------------------------

summary_stats <- plot_data_long |>
  group_by(scale, metric_label) |>
  summarise(
    n = n(),
    mean = mean(skill_score, na.rm = TRUE),
    median = median(skill_score, na.rm = TRUE),
    sd = sd(skill_score, na.rm = TRUE),
    min = min(skill_score, na.rm = TRUE),
    max = max(skill_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric) & !n, ~round(.x, 3)))

write.csv(summary_stats, 
          file.path(scale_run_folder, "summary_statistics.csv"), 
          row.names = FALSE)

cat("  ✓ Summary statistics saved\n")

# Print summary to console
cat("\n=== Summary Statistics ===\n")
print(summary_stats, n = Inf)

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("✅ SPATIAL SCALE COMPARISON COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("📂 All results saved to:", scale_run_folder, "\n\n")

cat("Contents:\n")
cat("  • base_config.rds - Configuration used for all runs\n")
cat("  • combined_metrics.rds - All metrics from all scales\n")
cat("  • summary_statistics.csv - Summary stats by scale and metric\n")
cat("  • combined_density_all_metrics.png - Density plots for all metrics\n")
cat("  • combined_jitter_all_metrics.png - Jitter plots for all metrics\n")
for (metric in skill_metrics) {
  metric_short <- toupper(gsub("_skill", "", metric))
  cat("  •", paste0(metric, "_density.png"), "-", metric_short, "density plot\n")
  cat("  •", paste0(metric, "_jitter.png"), "-", metric_short, "jitter plot\n")
}
cat("  • all/ - System-wide scale results\n")
cat("  • subregion/ - Subregional scale results\n")
cat("  • colony/ - Colony scale results\n")

cat("\n🎉 Done!\n\n")