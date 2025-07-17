# 03_unsup_pipeline/unsup_model_kmeans.R

## Build a function to apply k-means clustering
unsup_apply_kmeans <- function(pca_data, optimal_k) {
  set.seed(123)
  ## Apply k-means clustering
  km_result <- kmeans(pca_data[, 1:4], centers = optimal_k, nstart = 25)
  
  ## Plot clusters on PCA components
  pca_data$cluster <- as.factor(km_result$cluster)
  km_result
}
## Build a function to plot clusters
unsup_plot_clusters <- function(pca_data, km_result, optimal_k) {
  pca_data$cluster <- as.factor(km_result$cluster)
  library(ggplot2)
  cluster_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = paste("K-means Clustering with k =", optimal_k), x = "PC1", y = "PC2") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  cluster_plot
}
## Build a function to calculate silhouette score
unsup_calculate_silhouette <- function(km_result, pca_data) {
  library(cluster)
  sil_score <- silhouette(km_result$cluster, dist(pca_data[, 1:4]))
  avg_silhouette <- mean(sil_score[, 3])
  avg_silhouette
}
## Build a function to add silhouette score to the data frame
unsup_add_silhouette_score <- function(silhouette_scores, k, silhouette_score) {
  silhouette_scores <- rbind(silhouette_scores, data.frame(k = k, silhouette_score = silhouette_score))
  silhouette_scores
}
## Build a function to choose the best k based on silhouette score
unsup_choose_best_k <- function(silhouette_scores) {
  best_k <- silhouette_scores[which.max(silhouette_scores$silhouette_score), "k"]
  best_k
}
## Build a function to apply k-means until max_k to find optimal k
unsup_apply_kmeans_until_optimal <- function(pca_data, max_k) {
  silhouette_scores <- data.frame(k = integer(), silhouette_score = numeric())
  
  for (k in 2:max_k) {
    km_result <- unsup_apply_kmeans(pca_data, k)
    avg_silhouette <- unsup_calculate_silhouette(km_result, pca_data)
    silhouette_scores <- unsup_add_silhouette_score(silhouette_scores, k, avg_silhouette)
    cat("Silhouette Score for k =", k, ":", avg_silhouette, "\n")
  }
  ## Choose the best k based on silhouette scores
  best_k <- unsup_choose_best_k(silhouette_scores)
  best_km_result <- unsup_apply_kmeans(pca_data, best_k)
  best_silhouette_scores <- unsup_calculate_silhouette(best_km_result, pca_data)
  list(best_k, best_km_result, silhouette_scores)
}