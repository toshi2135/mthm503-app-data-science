# True class labels (1 = positive, 0 = negative)
true_labels <- c(1, 1, 0, 0, 1)

# Predicted probabilities from a classifier
predicted_probs <- c(0.9, 0.8, 0.6, 0.3, 0.5)


thresholds <- sort(unique(predicted_probs), decreasing = TRUE)
# Store TPR and FPR at each threshold
roc_points <- data.frame()

for (t in thresholds) {
  # Predicted class based on threshold
  predicted_class <- ifelse(predicted_probs >= t, 1, 0)
  
  # Confusion matrix components
  TP <- sum(predicted_class == 1 & true_labels == 1)
  FP <- sum(predicted_class == 1 & true_labels == 0)
  FN <- sum(predicted_class == 0 & true_labels == 1)
  TN <- sum(predicted_class == 0 & true_labels == 0)
  
  TPR <- TP / (TP + FN)  # Sensitivity
  FPR <- FP / (FP + TN)  # 1 - Specificity
  
  roc_points <- rbind(roc_points, data.frame(threshold = t, TPR, FPR))
}

# Add (0,0) and (1,1) for full curve
roc_points <- rbind(data.frame(threshold = 1.1, TPR = 0, FPR = 0), roc_points)
roc_points <- rbind(roc_points, data.frame(threshold = -0.1, TPR = 1, FPR = 1))



library(ggplot2)
roc_points <- roc_points[order(roc_points$FPR, roc_points$TPR), ]
rp <- roc_points[c(1:3),]
# Plot ROC curve with clean aesthetics
ggplot(rp, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  geom_text(aes(label = round(threshold, 2)), hjust = -0.1, vjust = -0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  coord_equal() +
  labs(title = "Conceptual ROC Curve",
       x = "False Positive Rate (FPR)",
       y = "True Positive Rate (TPR)") +
  theme_minimal() +
  xlim(c(0,1)) +
  ylim(c(0,1))

