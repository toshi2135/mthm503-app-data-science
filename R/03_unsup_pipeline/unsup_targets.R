# 03_unsup_pipeline/unsup_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/03_unsup_pipeline", "unsup_load_data.R"))
source(here("R/03_unsup_pipeline", "unsup_eda.R"))
source(here("R/03_unsup_pipeline", "unsup_preprocess.R"))
source(here("R/03_unsup_pipeline", "unsup_pca.R"))
source(here("R/03_unsup_pipeline", "unsup_model_kmeans.R"))
source(here("R/03_unsup_pipeline", "unsup_model_dbscan.R"))
source(here("R/03_unsup_pipeline", "unsup_model_hierarchical.R"))
source(here("R/03_unsup_pipeline", "unsup_summary.R"))

# Define the targets for the supervised learning pipeline
unsup_targets <- list(
  # Load data
  tar_target(unsup_raw_data, unsup_load_data()),
  # Explanatory Data Analysis
  tar_target(unsup_eda_results, unsup_eda(unsup_raw_data)),

  # Preprocess data
  tar_target(unsup_clean_data, unsup_preprocess(unsup_raw_data)),

  # Perform PCA
  tar_target(
    unsup_num_components,
    unsup_perform_pca(unsup_clean_data)$num_components
  ),
  tar_target(unsup_pca_data, unsup_perform_pca(unsup_clean_data)$pca_data),

  # Apply k-means clustering
  tar_target(
    unsup_kmeans_result,
    unsup_apply_optimal_kmeans(unsup_pca_data)$best_km_result
  ),
  tar_target(
    unsup_kmeans_sil_score,
    unsup_apply_optimal_kmeans(
      unsup_pca_data
    )$silhouette_scores
  ),

  # Apply DBSCAN clustering
  tar_target(
    unsup_dbscan_result,
    unsup_dbscan_apply(unsup_num_components, unsup_pca_data)$dbscan_result
  ),
  tar_target(
    unsup_dbscan_avg_sil_score,
    unsup_dbscan_apply(
      unsup_num_components,
      unsup_pca_data
    )$dbscan_avg_silhouette
  ),

  # Apply Hierarchical clustering
  tar_target(
    unsup_hierarchical_result,
    unsup_hier_apply(unsup_pca_data)$hc_result
  ),
  tar_target(
    unsup_hierarchical_avg_sil_score,
    unsup_hier_apply(unsup_pca_data)$hc_avg_silhouette
  ),

  # Compare clustering results
  tar_target(
    unsup_summary,
    unsup_summarise(
      unsup_pca_data,
      unsup_kmeans_result,
      unsup_kmeans_sil_score,
      unsup_dbscan_result,
      unsup_dbscan_avg_sil_score,
      unsup_hierarchical_result,
      unsup_hierarchical_avg_sil_score
    )
  )
)
