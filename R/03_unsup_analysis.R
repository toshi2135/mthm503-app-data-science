# Analyse the adulteration of olive oil using unsupervised methods

# Load libraries

# Load data
# Load libraries
library(DBI)
library(RPostgres)
library(here)
# Load load_data function
source(here("R", "load_data.R"))
# ---
# Data querying
## Use the .Renviron file to set the environment variables and connect to DB
## Read .Renviron file
readRenviron(".Renviron")
## Check if the environment variables are set
Sys.getenv("PGRHOST")
## Connect to the database using the load_data function
conn <- get_db_connection()
## Check the tables in the database
tables <- DBI::dbListTables(conn)
tables
## Check first few rows of olive_oil table
olive_oil <- DBI::dbReadTable(conn, "olive_oil")
## Check the structure of the data
str(olive_oil)
## Glimpse the first few rows
dplyr::glimpse(olive_oil)
## Check the summary of the data
summary(olive_oil)
## Retrieve data using SQL query
sql_query <- readLines(here("sql", "03_unsup_data_query.sql"))
query <- paste(sql_query, collapse = "\n")
olive_oil <- DBI::dbGetQuery(conn, query)
## Check the first few rows of the data
head(olive_oil)
## Close the connection
DBI::dbDisconnect(conn)
# ---

# Exploratory Data Analysis
library(ggplot2)
library(tidyr)
summary(olive_oil[, -1])
olive_long <- olive_oil %>%
  pivot_longer(cols = -id, names_to = "fatty_acid", values_to = "value")
## Plot the histogram of the fatty_acid column
ggplot(olive_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~fatty_acid, scales = "free", ncol = 3) +
  theme_minimal()
## Plot the scatter plot of the fatty_acid columns
library(GGally)
ggpairs(olive_oil[, -1])
## Plot the correlation matrix
library(corrplot)
cor_matrix <- cor(olive_oil[, -1])
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
## Check the outliers
boxplot(
  olive_oil[, -1],
  main = "Boxplot of Fatty Acids",
  las = 2,
  cex.axis = 0.7
)
# ---
# Data Preprocessing
library(dplyr)
## Remove ID column
olive_oil_clean <- olive_oil %>% select(-id)
## Standardise data
olive_oil_scaled <- olive_oil_clean %>%
  mutate(across(where(is.numeric), scale))
## Check the structure of the scaled data
str(olive_oil_scaled)
# ---

# Reduce PCA dimensions
library(stats)
pca_result <- prcomp(olive_oil_scaled, center = TRUE, scale. = TRUE)
## Check the summary of PCA
summary(pca_result)
## Analyse PCA
pca_var <- pca_result$sdev^2
pca_var_prop <- pca_var / sum(pca_var)
cum_var_prop <- cumsum(pca_var_prop)
## Plot the scree plot
plot(
  pca_var_prop,
  type = "b",
  pch = 19,
  xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  main = "Scree Plot (base R)",
  ylim = c(0, max(pca_var_prop) + 0.05)
)
abline(h = 0.1, col = "red", lty = 2)
## Biplot of PCA
biplot(pca_result, scale = 0, cex = 0.6, main = "Base R Biplot: PC1 vs PC2")
## Biplot of PCA using ggplot2
pca_points <- as.data.frame(pca_result$x)
pca_vars <- as.data.frame(pca_result$rotation)
pca_vars$varname <- rownames(pca_vars)
ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5) +
  geom_segment(
    data = pca_vars,
    aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "red"
  ) +
  geom_text(
    data = pca_vars,
    aes(x = PC1 * 5, y = PC2 * 5, label = varname),
    color = "red",
    vjust = -0.5,
    size = 3
  ) +
  labs(title = "Custom PCA Biplot", x = "PC1", y = "PC2") +
  theme_minimal()
## Plot PCA individual plots (PC1-PC2 scatter)
ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "PCA Individuals Plot", x = "PC1", y = "PC2") +
  theme_minimal()
## Plot PCA individual plots (PC1-PC3 scatter)
ggplot(pca_points, aes(x = PC1, y = PC3)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "PCA Individuals Plot (PC1 vs PC3)", x = "PC1", y = "PC3") +
  theme_minimal()
## Plot PCA individual plots (PC2-PC3 scatter)
ggplot(pca_points, aes(x = PC2, y = PC3)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "PCA Individuals: PC2 vs PC3", x = "PC2", y = "PC3") +
  theme_minimal()
target_variance <- 0.9
## Calculate the number of components needed to reach the target variance
num_components <- which(cum_var_prop >= target_variance)[1]
num_components # 4
## Reduce PCA dimensions to the number of components
pca_data <- as.data.frame(pca_result$x[, 1:num_components])
pca_data
# ---

# Apply k-means clustering
set.seed(123)
wss <- numeric(10)
## Compute k-means for k = 1 to 10
for (k in 1:10) {
  km_out <- kmeans(pca_data, centers = k, nstart = 25)
  wss[k] <- km_out$tot.withinss
}
## Plot the elbow method
plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (k)",
  ylab = "Total Within-Cluster Sum of Squares",
  main = "Elbow Method for Optimal k"
)
## Calculate the optimal k using gap statistic
library(cluster)
set.seed(123)
gap_stat <- clusGap(pca_data, FUN = kmeans, K.max = 10, B = 50)
plot(gap_stat)
which.max(gap_stat$Tab[, "gap"])
gap_df <- as.data.frame(gap_stat$Tab)
gap_df$k <- seq_len(nrow(gap_df))

for (i in 1:(nrow(gap_df) - 1)) {
  if (gap_df$gap[i] >= gap_df$gap[i + 1] - gap_df$SE.sim[i + 1]) {
    cat("Using 1-SE rule: choose k =", i, "\n")
    optimal_k <- i
    break
  }
}

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
    labs(
      title = paste("K-means Clustering with k =", optimal_k),
      x = "PC1",
      y = "PC2"
    ) +
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
  silhouette_scores <- rbind(
    silhouette_scores,
    data.frame(k = k, silhouette_score = silhouette_score)
  )
  silhouette_scores
}
## Build a function to choose the best k based on silhouette score
unsup_choose_best_k <- function(silhouette_scores) {
  best_k <- silhouette_scores[
    which.max(silhouette_scores$silhouette_score),
    "k"
  ]
  best_k
}
## Build a function to apply k-means until max_k to find optimal k
unsup_apply_optimal_kmeans <- function(pca_data, max_k) {
  silhouette_scores <- data.frame(k = integer(), silhouette_score = numeric())

  for (k in 2:max_k) {
    km_result <- unsup_apply_kmeans(pca_data, k)
    avg_silhouette <- unsup_calculate_silhouette(km_result, pca_data)
    silhouette_scores <- unsup_add_silhouette_score(
      silhouette_scores,
      k,
      avg_silhouette
    )
    cat("Silhouette Score for k =", k, ":", avg_silhouette, "\n")
  }
  ## Choose the best k based on silhouette scores
  best_k <- unsup_choose_best_k(silhouette_scores)
  best_km_result <- unsup_apply_kmeans(pca_data, best_k)
  best_silhouette_scores <- unsup_calculate_silhouette(best_km_result, pca_data)
  list(best_k, best_km_result, silhouette_scores)
}

optimal_k # 3

## Set a silhouette scores data frame
silhouette_scores <- data.frame(k = integer(), silhouette_score = numeric())

## Choose k = 3 based on elbow plot and gap statistic
# Apply k-means clustering with optimal k
km_result_k3 <- unsup_apply_kmeans(pca_data, optimal_k)
km_result_k3
## Plot clusters on PCA components
unsup_plot_clusters(pca_data, km_result_k3, optimal_k)
## Calculate silhouette score
silhouette_score <- unsup_calculate_silhouette(km_result_k3, pca_data)
## Add silhouette score to the data frame
silhouette_scores <- unsup_add_silhouette_score(
  silhouette_scores,
  optimal_k,
  silhouette_score
)
## Print silhouette score
cat("Silhouette Score for k =", optimal_k, ":", silhouette_score, "\n")
## Aggregate the data by cluster for k=3
olive_oil$cluster <- km_result_k3$cluster
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)

## Try with k=4
optimal_k4 <- optimal_k + 1
## Apply k-means clustering with k=4
km_result_k4 <- unsup_apply_kmeans(pca_data, optimal_k4)
## Plot clusters on PCA components for k=4
unsup_plot_clusters(pca_data, km_result_k4, optimal_k4)
## Calculate silhouette score for k=4
silhouette_score_k4 <- unsup_calculate_silhouette(km_result_k4, pca_data)
## Add silhouette score for k=4 to the data frame
silhouette_scores <- unsup_add_silhouette_score(
  silhouette_scores,
  optimal_k4,
  silhouette_score_k4
)
cat("Silhouette Score for k =", optimal_k4, ":", silhouette_score_k4, "\n")
## Aggregate the data by cluster for k=4
olive_oil$cluster <- km_result_k4$cluster
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)

## Try with k=5
optimal_k5 <- optimal_k + 2
## Apply k-means clustering with k=5
km_result_k5 <- unsup_apply_kmeans(pca_data, optimal_k5)
## Plot clusters on PCA components for k=5
unsup_plot_clusters(pca_data, km_result_k5, optimal_k5)
## Calculate silhouette score for k=5
silhouette_score_k5 <- unsup_calculate_silhouette(km_result_k5, pca_data)
## Add silhouette score for k=5 to the data frame
silhouette_scores <- unsup_add_silhouette_score(
  silhouette_scores,
  optimal_k5,
  silhouette_score_k5
)
cat("Silhouette Score for k =", optimal_k5, ":", silhouette_score_k5, "\n")
## Aggregate the data by cluster for k=5
olive_oil$cluster <- km_result_k5$cluster
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)

## Try with k=6
optimal_k6 <- optimal_k + 3
## Apply k-means clustering with k=6
km_result_k6 <- unsup_apply_kmeans(pca_data, optimal_k6)
## Plot clusters on PCA components for k=6
unsup_plot_clusters(pca_data, km_result_k6, optimal_k6)
## Calculate silhouette score for k=6
silhouette_score_k6 <- unsup_calculate_silhouette(km_result_k6, pca_data)
## Add silhouette score for k=6 to the data frame
silhouette_scores <- unsup_add_silhouette_score(
  silhouette_scores,
  optimal_k6,
  silhouette_score_k6
)
cat("Silhouette Score for k =", optimal_k6, ":", silhouette_score_k6, "\n")
## Aggregate the data by cluster for k=6
olive_oil$cluster <- km_result_k6$cluster
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)

## Use the function to loop through k and apply k-means clustering
### Get the number of k possible
max_k <- 10
### Apply k-means clustering until max_k
result <- unsup_apply_optimal_kmeans(pca_data, max_k)
best_k <- result[[1]]
best_km_result <- result[[2]]
best_silhouette_scores <- result[[3]]
## Print the best k with silhouette score
cat("Best k based on silhouette score:", best_k, "\n")
cat(
  "Best silhouette score:",
  max(best_silhouette_scores$silhouette_score),
  "\n"
)
## Calculate the average silhouette score for the best k
best_avg_silhouette <- unsup_calculate_silhouette(best_km_result, pca_data)
cat("Average Silhouette Score for best k:", best_avg_silhouette, "\n")
# ---

# Apply DBSCAN clustering
library(dbscan)
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
  labs(title = "DBSCAN Clustering on PCA (PC1 vs PC2)", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
## Initial analysis of DBSCAN clusters
table(pca_data$dbscan_cluster)
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)
## Calculate the silhouette score for DBSCAN clusters
dbscan_silhouette <- silhouette(dbscan_result$cluster, dist(pca_data))
dbscan_avg_silhouette <- mean(dbscan_silhouette[, 3])
cat("Average Silhouette Score for DBSCAN:", dbscan_avg_silhouette, "\n")
# ---

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
plot(
  2:10,
  sil_vals[2:10],
  type = "b",
  pch = 19,
  xlab = "Number of Clusters",
  ylab = "Average Silhouette Width"
)
best_k <- which.max(sil_vals)
cat(
  "Best k based on silhouette score for Hierarchical Clustering:",
  best_k,
  "\n"
)
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
## Calculate the silhouette score for Hierarchical clusters
hc_silhouette <- silhouette(pca_data$hc_cluster, dist(pca_data))
hc_avg_silhouette <- mean(hc_silhouette[, 3])
cat(
  "Average Silhouette Score for Hierarchical Clustering:",
  hc_avg_silhouette,
  "\n"
)
## Return the Hierarchical clustering result
hc_result <- pca_data$hc_cluster
# ---

# Compare clustering results based on silhouette scores
str(pca_data)
str(best_km_result)
str(dbscan_result)
str(hc_result)
km_sil_score <- best_avg_silhouette
km_sil_score
dbscan_sil_score <- dbscan_avg_silhouette
dbscan_sil_score
hc_sil_score <- hc_avg_silhouette
hc_sil_score
## Create a summary data frame to compare methods
## Using average silhouette scores and number of clusters
library(dplyr)
## Calculate the number of clusters for each method
km_clusters <- length(unique(best_km_result$cluster))
km_clusters
dbscan_clusters <- length(unique(dbscan_result$cluster))
dbscan_clusters
hc_clusters <- length(unique(hc_result))
hc_clusters
## Create a summary data frame
results <- data.frame(
  Method = c("K-means", "DBSCAN", "Hierarchical"),
  Avg_Silhouette_Score = c(km_sil_score, dbscan_sil_score, hc_sil_score),
  Num_Clusters = c(km_clusters, dbscan_clusters, hc_clusters)
)
results
