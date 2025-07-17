# 03_unsup_pipeline/unsup_model_dbscan.R

unsup_dbscan_apply <- function(num_components, pca_data) {
  # Apply DBSCAN clustering
  library(dbscan)
  library(ggplot2)
  library(cluster)
  set.seed(123)
  ## Get the data for DBSCAN
  dbscan_data <- pca_data
  ## Determine min_pts for DBSCAN
  min_pts <- num_components + 1
  ## minPts = d + 1, where d is the number of dimensions
  ## Determine eps using kNNdistplot
  kNNdistplot(dbscan_data, k = min_pts)
  abline(h = 1.2, col = "red", lty = 2)
  title(main = "kNN Distance Plot (k=4) for DBSCAN eps selection")
  ## Apply DBSCAN with eps = 1.2 and minPts = 4
  dbscan_result <- dbscan(dbscan_data, eps = 1.2, minPts = 4)
  pca_data$cluster <- as.factor(dbscan_result$cluster)
  ## Plot DBSCAN clusters on PC1 vs PC2
  ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      title = "DBSCAN Clustering on PCA (PC1 vs PC2)",
      x = "PC1",
      y = "PC2"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  ## Initial analysis of DBSCAN clusters
  table(pca_data$dbscan_cluster)
  ## Calculate the silhouette score for DBSCAN clusters
  dbscan_silhouette <- silhouette(dbscan_result$cluster, dist(pca_data))
  dbscan_avg_silhouette <- mean(dbscan_silhouette[, 3])
  cat("Average Silhouette Score for DBSCAN:", dbscan_avg_silhouette, "\n")
  ## Return the DBSCAN result
  list(
    dbscan_result = dbscan_result,
    dbscan_avg_silhouette = dbscan_avg_silhouette
  )
}
