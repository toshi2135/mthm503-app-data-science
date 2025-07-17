# 03_unsup_pipeline/unsup_eda.R

unsup_eda <- function(olive_oil) {
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
}
