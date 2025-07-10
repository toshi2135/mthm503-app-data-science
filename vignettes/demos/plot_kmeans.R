# Load required library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data (two Gaussian blobs)
n <- 100
data <- data.frame(
  x = c(rnorm(n, mean = 0), rnorm(n, mean = 5)),
  y = c(rnorm(n, mean = 0), rnorm(n, mean = 5))
)

# Number of clusters
K <- 2

# Randomly initialize cluster centers
centers <- data[sample(1:nrow(data), K), ]

# Helper to compute assignments (E-step)
assign_clusters <- function(data, centers) {
  dists <- as.matrix(dist(rbind(centers, data)))[1:K, (K+1):(K+nrow(data))]
  apply(dists, 2, which.min)
}

# Helper to update centers (M-step)
update_centers <- function(data, assignments) {
  do.call(rbind, lapply(1:K, function(k) {
    colMeans(data[assignments == k, ])
  }))
}

# Helper to plot
plot_kmeans <- function(data, centers, assignments, iter) {
  data$cluster <- as.factor(assignments)
  centers_df <- data.frame(centers, cluster = as.factor(1:K))
  
  ggplot(data, aes(x = x, y = y, color = cluster)) +
    geom_point(size = 2, alpha = 0.5) +  # semi-transparent points
    geom_point(data = centers_df, aes(x = x, y = y),
               color = "black", fill = "yellow",
               shape = 21, size = 6, stroke = 2) +  # bold centers
    ggtitle(paste("Iteration", iter)) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"))
}

plot_null <- function(data, centers, assignments, iter) {
  data$cluster <- as.factor(assignments)
  centers_df <- data.frame(centers, cluster = as.factor(1:K))
  
  ggplot(data, aes(x = x, y = y)) +
    geom_point(size = 2, color = "grey", alpha = 0.5) +  # semi-transparent points
    geom_point(data = centers_df, aes(x = x, y = y),
               color = "black", fill = "yellow",
               shape = 21, size = 6, stroke = 2) +  # bold centers
    ggtitle(paste("Iteration", iter)) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"))
}


# Run k-means manually, step-by-step
max_iters <- 5
assignments <- NULL
centers <- data[sample(1:nrow(data), K), ]
for (iter in 1:max_iters) {
  # E-step: assign clusters
  assignments <- assign_clusters(data[, c("x", "y")], centers)
  
  print(plot_null(data, centers, assignments, iter))
  
  readline("Press [Enter] to continue to next iteration...")
  
  # Plot after E-step
  print(plot_kmeans(data, centers, assignments, iter))
  
  # M-step: update centers
  centers <- update_centers(data[, c("x", "y")], assignments)
  
  # Pause between iterations
  readline("Press [Enter] to continue to next iteration...")
}




# Load required library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data (same as before)
n <- 100
data <- data.frame(
  x = c(rnorm(n, mean = 0), rnorm(n, mean = 5)),
  y = c(rnorm(n, mean = 0), rnorm(n, mean = 5))
)

# Compute distance matrix
d <- dist(data)

# Perform hierarchical clustering
hc <- hclust(d, method = "complete")  # You can try 'single', 'average', etc.

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.6)

# Choose a cut height or number of clusters
k <- 2
clusters <- cutree(hc, k = k)

# Plot the data with clusters
data$cluster <- as.factor(clusters)

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 2) +
  ggtitle(paste("Hierarchical Clustering (cut at", k, "clusters)")) +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))




# Load required libraries
library(ggplot2)
library(gridExtra)

set.seed(123)

# Dataset with true clusters (2 Gaussian blobs)
n <- 100
data_clusters <- data.frame(
  x = c(rnorm(n, mean = 0), rnorm(n, mean = 5)),
  y = c(rnorm(n, mean = 0), rnorm(n, mean = 5))
)

# Dataset without true clusters (uniform)
data_partition <- data.frame(
  x = runif(2 * n, min = 0, max = 5),
  y = runif(2 * n, min = 0, max = 5)
)

# Apply k-means with k=2 to both
k <- 2
km_clusters <- kmeans(data_clusters, centers = k)
km_partition <- kmeans(data_partition, centers = k)

# Add cluster assignments
data_clusters$cluster <- as.factor(km_clusters$cluster)
data_partition$cluster <- as.factor(km_partition$cluster)

# Plot with true clusters
p1 <- ggplot(data_clusters, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 2) +
  ggtitle("Clustering: True Cluster Structure") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))

# Plot with uniform data (partition)
p2 <- ggplot(data_partition, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 2) +
  ggtitle("Partitioning: No Natural Clusters") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))

# Display side-by-side
grid.arrange(p1, p2, ncol = 2)

