# Analyse the adulteration of olive oil using unsupervised methods

# Load libraries

# Load data
# Load libraries
library(DBI)
library(RPostgres)
library(stats19)
# Load load_data function
source("R/load_data.R")
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
  facet_wrap(~ fatty_acid, scales = "free", ncol = 3) +
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
## Reduce PCA dimensions
library(stats)
pca_result <- prcomp(olive_oil_scaled, center = TRUE, scale. = TRUE)
## Check the summary of PCA
summary(pca_result)
## Analyse PCA
pca_var <- pca_result$sdev^2
pca_var_prop <- pca_var / sum(pca_var)
## Plot the scree plot
plot(pca_var_prop, type = "b", pch = 19,
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot (base R)",
     ylim = c(0, max(pca_var_prop) + 0.05))
abline(h = 0.1, col = "red", lty = 2)
## Biplot of PCA
biplot(pca_result, scale = 0,
       cex = 0.6,
       main = "Base R Biplot: PC1 vs PC2")
## Biplot of PCA using ggplot2
pca_points <- as.data.frame(pca_result$x)
pca_vars <- as.data.frame(pca_result$rotation)
pca_vars$varname <- rownames(pca_vars)
ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5) +
  geom_segment(data = pca_vars, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
               arrow = arrow(length = unit(0.2, "cm")), colour = "red") +
  geom_text(data = pca_vars, aes(x = PC1 * 5, y = PC2 * 5, label = varname),
            color = "red", vjust = -0.5, size = 3) +
  labs(title = "Custom PCA Biplot",
       x = "PC1", y = "PC2") +
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
# ---

# Apply k-means clustering
set.seed(123)
## Use first 4 principal components for clustering
pca_data <- as.data.frame(pca_result$x[, 1:4])
wss <- numeric(10)
## Compute k-means for k = 1 to 10
for (k in 1:10) {
  km_out <- kmeans(pca_data, centers = k, nstart = 25)
  wss[k] <- km_out$tot.withinss
}
## Plot the elbow method
plot(1:10,
     wss,
     type = "b",
     pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")
## Choose k = 3 based on elbow plot
optimal_k <- 3
## Plot clusters on PCA components 
km_result <- kmeans(pca_data[, 1:4], centers = optimal_k, nstart = 25)
## Plot clusters on PC1 vs PC2
pca_data$cluster <- as.factor(km_result$cluster)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "K-means (k=3) on PCA (PC1 vs PC2)", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
## Calculate silhouette score
library(cluster)
sil_score_3 <- silhouette(km_result$cluster, dist(pca_data[, 1:4]))
avg_silhouette <- mean(sil_score_3[, 3])
cat("Average Silhouette Score for k =", optimal_k, "is", avg_silhouette, "\n")
## Check the average values of original features per cluster
olive_oil$cluster <- km_result$cluster
aggregate(. ~ cluster, data = olive_oil[, -1], FUN = mean)
## Try with k=4
optimal_k_4 <- 4
km_result_4 <- kmeans(pca_data[, 1:4], centers = optimal_k_4, nstart = 25)
pca_data$cluster_4 <- as.factor(km_result_4$cluster)
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster_4)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "K-means (k=4) on PCA (PC1 vs PC2)", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
## Calculate silhouette score for k=4
sil_score_4 <- silhouette(km_result_4$cluster, dist(pca_data[, 1:4]))
avg_silhouette_4 <- mean(sil_score_4[, 3])
cat("Average Silhouette Score for k =", optimal_k_4, "is", avg_silhouette_4, "\n")
## Check the average values of original features per cluster for k=4
olive_oil$cluster_4 <- km_result_4$cluster
aggregate(. ~ cluster_4, data = olive_oil[, -1], FUN = mean)
# ---

# Apply DBSCAN clustering
library(dbscan)
set.seed(123)
## Get the data for DBSCAN
dbscan_data <- pca_data[, 1:4]
## Determine eps using kNNdistplot
kNNdistplot(dbscan_data, k = 4)
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
# ---

# Apply Hierarchical Clustering
set.seed(123)
## Prepare the data
hclust_data <- pca_data[, 1:4]
## Compute distance matrix
dist_mat <- dist(hclust_data)
## Clustering using Ward method
hc_model <- hclust(dist_mat, method = "ward.D2")
## Plot dendrogram
plot(hc_model, labels = FALSE, hang = -1,
     main = "Hierarchical Clustering Dendrogram")
abline(h = 10, col = "red", lty = 2)
## Cut tree to get 4 clusters
pca_data$hc_cluster <- cutree(hc_model, k = 4)
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
# ---

# Check the heatmap of the clusters
library(tibble)
## Prepare data for heatmap
heatmap_data <- olive_oil %>%
  group_by(hc_cluster) %>%
  summarise(across(palmitic:eicosenoic, mean)) %>%
  column_to_rownames("hc_cluster") %>%
  as.matrix()
## Plot the heatmap
heatmap(heatmap_data,
        Colv = NA, Rowv = NA,
        scale = "column",
        col = colorRampPalette(c("white", "orange", "red"))(100),
        margins = c(8, 6),
        main = "Heatmap of Fatty Acid Composition by Cluster")
# ---