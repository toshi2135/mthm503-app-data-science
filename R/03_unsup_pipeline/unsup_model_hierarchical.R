# 03_unsup_pipeline/unsup_model_hierarchical.R

unsup_hier_apply <- function(pca_data) {
  # Apply Hierarchical Clustering
  set.seed(123)
  ## Prepare the data
  hclust_data <- pca_data
  ## Compute distance matrix
  dist_mat <- dist(hclust_data)
  ## Clustering using Ward method
  hc_model <- hclust(dist_mat, method = "ward.D2")
  ## Plot dendrogram
  plot(
    hc_model,
    labels = FALSE,
    hang = -1,
    main = "Hierarchical Clustering Dendrogram"
  )
  abline(h = 10, col = "red", lty = 2)
  ## Choose the k for cutting the tree
  library(cluster)
  sil_vals <- numeric()
  for (k in 2:10) {
    cluster_k <- cutree(hc_model, k = k)
    sil <- silhouette(cluster_k, dist(pca_data[, 1:4]))
    sil_vals[k] <- mean(sil[, 3])
  }
  plot(2:10, sil_vals[2:10], type = "b", pch = 19,
       xlab = "Number of Clusters", ylab = "Average Silhouette Width")
  best_k <- which.max(sil_vals)
  cat("Best k based on silhouette score for Hierarchical Clustering:", best_k, "\n")
  ## Cut tree to get k clusters
  pca_data$hc_cluster <- cutree(hc_model, k = best_k)
  ## Add into olive_oil data
  olive_oil$hc_cluster <- pca_data$hc_cluster
  ## Plot Hierarchical clusters on PC1 vs PC2
  ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(hc_cluster))) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "Hierarchical Clustering (k=4) on PCA", color = "Cluster") +
    theme_minimal()
  ## Initial analysis of Hierarchical clusters
  table(pca_data$hc_cluster)
  aggregate(. ~ hc_cluster, data = olive_oil[, -1], FUN = mean)
  ## Return the Hierarchical clustering result
  hc_model
}