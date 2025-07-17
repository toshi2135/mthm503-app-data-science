# 03_sup_pipeline/unsup_pca.R

unsup_perform_pca <- function(olive_oil_scaled) {
  # Reduce PCA dimensions
  library(stats)
  library(ggplot2)
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
  ## Reduce PCA dimensions to number of components
  pca_data <- as.data.frame(pca_result$x[, 1:num_components])
  colnames(pca_data) <- paste0("PC", 1:num_components)
  ## Check the PCA data structure
  str(pca_data)
  ## Convert pca_data to dataframe
  pca_data <- as.data.frame(pca_data)
  ## Return the number of components and the PCA data
  list(
    num_components = num_components,
    pca_data = pca_data
  )
}
