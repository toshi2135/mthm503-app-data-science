# 03_unsup_pipeline/unsup_summary.R

## Create a function to summarise clustering results
unsup_summarise <- function(
  pca_data,
  best_km_result,
  km_sil_score,
  dbscan_result,
  dbscan_sil_score,
  hc_result,
  hc_sil_score
) {
  library(dplyr)
  ## Calculate the number of clusters for each method
  km_clusters <- length(unique(best_km_result$cluster))
  cat("Number of clusters in K-means:", km_clusters, "\n")
  dbscan_clusters <- length(unique(dbscan_result$cluster))
  cat("Number of clusters in DBSCAN:", dbscan_clusters, "\n")
  hc_clusters <- length(unique(hc_result))
  cat("Number of clusters in Hierarchical clustering:", hc_clusters, "\n")

  ## Create a summary data frame
  results <- data.frame(
    Method = c("K-means", "DBSCAN", "Hierarchical"),
    Avg_Silhouette_Score = c(km_sil_score, dbscan_sil_score, hc_sil_score),
    Num_Clusters = c(km_clusters, dbscan_clusters, hc_clusters)
  )
  results
}
