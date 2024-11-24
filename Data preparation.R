# Install and load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(tidyverse)
library(mlbench)
library(corrplot)
library(MASS)
library(HSAUR2)
library(outliers)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(corrplot)



# Question - 1a (i)
# Create the correlation matrix of all the numerical attributes in the Glass data and store the results in a new object corMat.
# Load the Glass dataset from mlbench

data("Glass")
glass_data <- Glass

# Ensure the data is loaded correctly
head(glass_data)

# Check for duplicate rows
duplicate_rows_before <- glass_data[duplicated(glass_data), ]
num_duplicates_before <- nrow(duplicate_rows_before)
cat("Number of duplicate records before removal:",
    num_duplicates_before,
    "\n")

# Remove duplicate rows
glass_data_clean <- glass_data %>% distinct()

# Check for duplicate rows after removal
duplicate_rows_after <- glass_data_clean[duplicated(glass_data_clean), ]
num_duplicates_after <- nrow(duplicate_rows_after)
cat("Number of duplicate records after removal:",
    num_duplicates_after,
    "\n")

# Exclude the "Type" column and create the correlation matrix
corMat <- cor(glass_data_clean[, -which(names(glass_data_clean) == "Type")])

cat("Correlation Matrix:\n")
print(corMat)

#-----------------------------------------------------------------------------------

# Question - 1a (ii)
# Compute the eigenvalues and eigenvectors of corMat.

eigen_result <- eigen(corMat)

# Extract eigenvalues and eigenvectors
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors


cat("Eigenvalues:\n")
print(eigenvalues)

cat("Eigenvectors:\n")
print(eigenvectors)

#-----------------------------------------------------------------------------------

# Question - 1a (iii)
# Use prcomp to compute the principal components of the Glass attributes (make sure to use the scale option).

glass_attributes <- glass_data_clean[, !(names(glass_data_clean) %in% "Type")]

# Perform PCA using prcomp with scaling
pca_result <- prcomp(glass_attributes, scale. = TRUE)

# Extract PCA results
pca_sdev <- pca_result$sdev
pca_rotation <- pca_result$rotation
pca_scores <- pca_result$x

cat("PCA Standard Deviations:\n")
print(pca_sdev)

cat("Principal Components (Rotation):\n")
print(pca_rotation)

cat("Principal Component Scores:\n")
print(pca_scores)

#------------------------------------------------------------------------------------
# Question - 1a (iv)
# Comparison of results from (ii) and (iii)

# Compare eigenvalues and PCA standard deviations squared
pca_variances <- pca_sdev ^ 2
eigenvalues_comparison <- all.equal(eigenvalues, pca_variances)
cat("Eigenvalues and PCA Standard Deviations Squared Comparison:\n")
print(eigenvalues_comparison)

# Compare eigenvectors (PCA loadings)
# Sort eigenvectors and PCA loadings by the norm of each column to ensure consistent ordering
eigenvectors_sorted <- eigenvectors[, order(apply(eigenvectors, 2, function(x)
  sqrt(sum(x ^ 2))))]
pca_rotation_sorted <- pca_rotation[, order(apply(pca_rotation, 2, function(x)
  sqrt(sum(x ^ 2))))]

# Ensure both matrices have the same number of columns
common_columns <- min(ncol(eigenvectors_sorted), ncol(pca_rotation_sorted))

# Subset both matrices to the minimum number of columns
eigenvectors_sorted <- eigenvectors_sorted[, 1:common_columns]
pca_rotation_sorted <- pca_rotation_sorted[, 1:common_columns]

# Remove attributes for a fair comparison
attributes(eigenvectors_sorted) <- NULL
attributes(pca_rotation_sorted) <- NULL

# Compare eigenvectors (PCA loadings) after fixing the length mismatch
eigenvectors_comparison <- all.equal(abs(eigenvectors_sorted),
                                     abs(pca_rotation_sorted),
                                     tolerance = 1e-10)
cat("Eigenvectors and PCA Loadings Comparison:\n")
print(eigenvectors_comparison)




#------------------------------------------------------------------------------------
# Question - 1a (v). Using R demonstrate that principal components 1 and 2 from (iii) are orthogonal. (Hint: the
#inner product between two vectors is useful in determining the angle between the two vectors)

# Assuming pca_result from step (iii) is already available

# Extract Principal Components 1 and 2
pc1 <- pca_result$rotation[, 1]
pc2 <- pca_result$rotation[, 2]

# Compute the dot product of Principal Components 1 and 2
dot_product <- sum(pc1 * pc2)

cat("Dot Product of Principal Components 1 and 2:\n")
print(dot_product)

# Check if the dot product is close to zero (indicating orthogonality)
tolerance <- 1e-10  # Define a tolerance level for numerical precision
is_orthogonal <- abs(dot_product) < tolerance
cat("Are Principal Components 1 and 2 orthogonal?\n")
print(is_orthogonal)

#-------------------------------------------------------------------------------------------------
# Question - 1b Application of PCA
#i. Create a visualization of the corMat correlation matrix (i.e., a heatmap or variant) If you
#are interested and have time, consider the corrplot package for very nice options, https:
#  //cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html.

# Create a visualization of the correlation matrix using corrplot
corrplot(
  corMat,
  method = "color",
  col = colorRampPalette(c("blue", "white", "red"))(200),
  title = "Correlation Matrix of Glass Data",
  tl.cex = 0.8,
  # Text size for axis labels
  addCoef.col = "black",
  # Show correlation values
  number.cex = 0.7,
  # Text size for correlation coefficients
  cl.cex = 0.8,
  # Text size for color legend
  addgrid.col = "gray",
  # Grid color
  mar = c(0, 0, 1, 0)
) # Margins



#-------------------------------------------------------------------------------------------------
# Question - 1b ii. Provide visualizations of the principal component analysis results from the Glass data. Con
# sider incorporating the glass type to group and color your biplot.


# Extract the PCA scores (principal components) and glass types
pca_scores <- data.frame(pca_result$x)
pca_scores$Type <- glass_data_clean$Type

# Create a biplot with color grouping by glass type and include variable loadings
ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(Type))) +
  geom_point(size = 3) +
  geom_segment(
    data = as.data.frame(pca_rotation),
    aes(
      x = 0,
      y = 0,
      xend = PC1 * 5,
      yend = PC2 * 5
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "black"
  ) +
  geom_text(
    data = as.data.frame(pca_rotation),
    aes(
      x = PC1 * 5,
      y = PC2 * 5,
      label = rownames(pca_rotation)
    ),
    color = "black",
    vjust = 1,
    hjust = 1
  ) +
  stat_ellipse(aes(group = Type), type = "norm", level = 0.95) +  # Adding confidence ellipses
  theme_minimal() +
  labs(
    title = "PCA Biplot of Glass Data",
    x = paste(
      "Principal Component 1 (",
      round(pca_result$sdev[1] ^ 2 / sum(pca_result$sdev ^ 2) * 100, 1),
      "% variance)",
      sep = ""
    ),
    y = paste(
      "Principal Component 2 (",
      round(pca_result$sdev[2] ^ 2 / sum(pca_result$sdev ^ 2) * 100, 1),
      "% variance)",
      sep = ""
    ),
    color = "Glass Type"
  ) +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Glass Type")



#-------------------------------------------------------------------------------------------------

# Question - 1b iii. Provide an interpretation of the rst two prinicpal components the Glass data.

# Perform PCA
pca_results <- prcomp(glass_attributes, scale. = TRUE)

# Extract the loadings matrix
loadings <- pca_results$rotation

print("Loadings Matrix:")
print(loadings)

# Determine the variables with the highest absolute loadings for PC1 and PC2
pc1_loadings <- loadings[, "PC1"]
pc2_loadings <- loadings[, "PC2"]

# Sort loadings by absolute value
sorted_pc1_loadings <- sort(abs(pc1_loadings), decreasing = TRUE)
sorted_pc2_loadings <- sort(abs(pc2_loadings), decreasing = TRUE)

print("Variables contributing most to PC1:")
for (variable in names(sorted_pc1_loadings)) {
  print(paste(variable, ":", pc1_loadings[variable]))
}

print("Variables contributing most to PC2:")
for (variable in names(sorted_pc2_loadings)) {
  print(paste(variable, ":", pc2_loadings[variable]))
}


top_contributors_pc1 <- names(sorted_pc1_loadings)[1:5]  # Top 5 contributors to PC1
top_contributors_pc2 <- names(sorted_pc2_loadings)[1:5]  # Top 5 contributors to PC2

print("Top 5 contributors to PC1:")
print(top_contributors_pc1)

print("Top 5 contributors to PC2:")
print(top_contributors_pc2)





# Perform PCA
pca_results <- prcomp(glass_attributes, scale. = TRUE)

# Calculate the proportion of variance explained by each principal component
variance_explained <- pca_results$sdev ^ 2 / sum(pca_results$sdev ^ 2)

# Convert to percentages
variance_percentages <- variance_explained * 100

# Display the variance explained by the first two components
variance_percentages[1:2]







#-------------------------------------------------------------------------------------------------
# Question - 1b (iv) Based on the PCA results, do you believe that you can e ectively reduce the dimension of the
# data? If so, to what degree? If not, why?


# Perform PCA using prcomp with scaling (assuming glass_attributes is the dataset with numerical variables)
pca_result <- prcomp(glass_attributes, scale. = TRUE)

# Calculate the variance explained by each principal component
pca_variance_explained <- (pca_result$sdev ^ 2) / sum(pca_result$sdev ^
                                                        2)

# Calculate cumulative variance explained
cumulative_variance_explained <- cumsum(pca_variance_explained)

# Determine the number of components needed to explain at least 90% variance
threshold <- 0.9
num_components <- min(which(cumulative_variance_explained >= threshold))

# Generate interpretation string
if (num_components < ncol(glass_attributes)) {
  interpretation <- paste(
    "Based on the PCA results, you can effectively reduce the dimensionality of the data to",
    num_components,
    "principal components, which explain approximately",
    round(cumulative_variance_explained[num_components] * 100, 1),
    "% of the total variance."
  )
} else {
  interpretation <- "Based on the PCA results, you cannot effectively reduce the dimensionality of the data because almost all components are needed to retain the variance."
}

cat(interpretation, "\n")

cat("Variance explained by each principal component:\n")
for (i in 1:length(pca_variance_explained)) {
  cat(paste0(
    "PC",
    i,
    ": ",
    round(pca_variance_explained[i] * 100, 1),
    "% of variance\n"
  ))
}

# Plot: Scree Plot
ggplot(data.frame(
  PC = 1:length(pca_variance_explained),
  Variance = pca_variance_explained
),
aes(x = PC, y = Variance)) +
  geom_point() +
  geom_line() +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

# Plot: Cumulative Variance Explained
ggplot(
  data.frame(
    PC = 1:length(cumulative_variance_explained),
    CumulativeVariance = cumulative_variance_explained
  ),
  aes(x = PC, y = CumulativeVariance)
) +
  geom_point() +
  geom_line() +
  labs(title = "Cumulative Variance Explained", x = "Principal Component", y = "Cumulative Proportion of Variance Explained") +
  geom_hline(yintercept = threshold,
             linetype = "dashed",
             color = "red") +
  theme_minimal()

cat("Cumulative variance explained by principal components:\n")
print(cumulative_variance_explained)




#-------------------------------------------------------------------------------------------------
# Question - 1c Application of LDA
#i. Since the Glass data is grouped into various labeled glass types we can consider linear discrim
#inant analysis (LDA) as another form of dimension reduction. Use the lda method from the
#MASS package to reduce the Glass data dimensionality.

# Separate the predictor variables (features) and the response variable (class labels)
glass_features <- glass_data_clean[, !(names(glass_data_clean) %in% "Type")]
glass_type <- glass_data_clean$Type


# Perform LDA
lda_result <- lda(Type ~ ., data = glass_data_clean)

print(lda_result)

cat("Linear Discriminants:\n")
print(lda_result$scaling)

# Proportion of trace (variance explained by each discriminant)
cat("Proportion of variance explained by each discriminant:\n")
print(lda_result$svd ^ 2 / sum(lda_result$svd ^ 2))




# Predict LDA scores
lda_scores <- predict(lda_result)$x

# Plot LDA scores
plot(
  lda_scores,
  col = as.factor(glass_data_clean$Type),
  pch = 19,
  main = "LDA of Glass Data",
  xlab = "First Linear Discriminant",
  ylab = "Second Linear Discriminant"
)

legend(
  "topright",
  legend = levels(as.factor(glass_data_clean$Type)),
  col = 1:length(levels(as.factor(
    glass_data_clean$Type
  ))),
  pch = 19
)


#-------------------------------------------------------------------------------------------------
# Question - 1c iii. Use the ldahist function from the MASS package to visualize the results for LD1 and LD2.
#Comment on the results.

# Close all open graphics devices to reset the graphics state
graphics.off()

# Open a new graphics window (use x11() on Linux, windows() on Windows, quartz() on macOS)
# Uncomment the appropriate line for your OS
# x11(width = 10, height = 8)   # Linux
windows(width = 10, height = 8) # Windows
# quartz(width = 10, height = 8)  # macOS

# Adjust plot margins
par(mar = c(5, 4, 4, 2) + 0.1)

# Visualize the distribution of LD1
ldahist(
  data = lda_scores[, 1],
  g = glass_data_clean$Type,
  main = "Histogram of LD1 by Glass Type",
  xlab = "Linear Discriminant 1 (LD1)"
)

# Visualize the distribution of LD2
ldahist(
  data = lda_scores[, 2],
  g = glass_data_clean$Type,
  main = "Histogram of LD2 by Glass Type",
  xlab = "Linear Discriminant 2 (LD2)"
)

# Save the plot of LD1 to a PNG file with appropriate dimensions
png(
  "LD1_histogram.png",
  width = 1200,
  height = 900,
  res = 100
)
par(mar = c(5, 4, 4, 2) + 0.1) # Set margins for the saved plot
ldahist(
  data = lda_scores[, 1],
  g = glass_data_clean$Type,
  main = "Histogram of LD1 by Glass Type",
  xlab = "Linear Discriminant 1 (LD1)"
)
dev.off()

# Save the plot of LD2 to a PNG file with appropriate dimensions
png(
  "LD2_histogram.png",
  width = 1200,
  height = 900,
  res = 100
)
par(mar = c(5, 4, 4, 2) + 0.1) # Set margins for the saved plot
ldahist(
  data = lda_scores[, 2],
  g = glass_data_clean$Type,
  main = "Histogram of LD2 by Glass Type",
  xlab = "Linear Discriminant 2 (LD2)"
)
dev.off()



#-------------------------------------------------------------------------------------------------



# Question 2 Principal components for dimension reduction
#(a) (10 points) Examine the event results using the Grubbs test. According to this test there is one
#competitor who is an outlier multiple events: Who is the competitor? And for which events is there
#statistical evidence that she is an outlier? Remove her from the data.


data("heptathlon", package = "HSAUR2")

# View the dataset to understand its structure
head(heptathlon)

# Grubbs' test to find outliers for each event
events <- colnames(heptathlon)[1:7]  # The first 7 columns are the events
outliers <- list()  # To store outliers for each event

# Perform Grubbs' test for each event
for (event in events) {
  test_result <- grubbs.test(heptathlon[[event]])
  if (test_result$p.value < 0.05) {
    # If p-value is less than 0.05, consider it an outlier
    outliers[[event]] <- test_result
  }
}

cat("Grubbs' Test Results:\n")
for (event in names(outliers)) {
  cat(event, ":", "p-value =", outliers[[event]]$p.value, "\n")
}

# Identify the competitor who is an outlier in multiple events
outlier_competitors <- c()
for (event in names(outliers)) {
  outlier_value <- outliers[[event]]$statistic[[1]]  # The Grubbs' statistic for the event
  outlier_competitor <- rownames(heptathlon)[heptathlon[[event]] == max(heptathlon[[event]], na.rm = TRUE)]
  outlier_competitors <- c(outlier_competitors, outlier_competitor)
}

# Find the competitor who is an outlier in multiple events
outlier_competitor <- names(sort(table(outlier_competitors), decreasing = TRUE))[1]

cat("The competitor who is an outlier in multiple events is:",
    outlier_competitor,
    "\n")

# Determine the events where the competitor is an outlier
outlier_events <- names(outliers)[sapply(outliers, function(x)
  any(rownames(heptathlon) == outlier_competitor))]
cat(
  "Events where the competitor is an outlier:\n",
  paste(outlier_events, collapse = ", "),
  "\n"
)

# Remove the outlier competitor from the data
heptathlon_clean <- heptathlon[rownames(heptathlon) != outlier_competitor, , drop = FALSE]

cat("Cleaned dataset:\n")
head(heptathlon_clean)

# Check how many records were removed
original_count <- nrow(heptathlon)
cleaned_count <- nrow(heptathlon_clean)
records_removed <- original_count - cleaned_count
cat("Number of records removed:", records_removed, "\n")
cat("Number of records remaining after cleaning:",
    cleaned_count,
    "\n")


#-------------------------------------------------------------------------------------------------

# Question 2 (b) #As is, some event results are good if the values are large (e.g. highjump), but some
#are bad if the value is large (e.g. time to run the 200 meter dash). Transform the running events
#(hurdles, run200m, run800m) so that large values are good. An easy way to do this is to subtract
#values from the max value for the event, i.e. xi
#xmax xi


# Apply the transformation: max(event) - value
heptathlon_clean$hurdles <- max(heptathlon_clean$hurdles) - heptathlon_clean$hurdles
heptathlon_clean$run200m <- max(heptathlon_clean$run200m) - heptathlon_clean$run200m
heptathlon_clean$run800m <- max(heptathlon_clean$run800m) - heptathlon_clean$run800m

# Display the transformed dataset
cat("Transformed dataset with running events inverted:\n")
head(heptathlon_clean)


#-------------------------------------------------------------------------------------------------
# Question 2 (c) (5 points) Perform a principal component analysis on the 7 event results and save the results of the
#prcomp function to a new variable Hpca.



# Perform PCA on the 7 event results using prcomp
# Select only the 7 event columns for PCA
event_data <- heptathlon_clean[, 1:7]

# Perform PCA and save the result to a variable 'Hpca'
Hpca <- prcomp(event_data, scale. = TRUE)

cat("Summary of Principal Component Analysis (PCA):\n")
summary(Hpca)

cat("PCA Results (Hpca):\n")
print(Hpca)

#-------------------------------------------------------------------------------------------------

# Question 2 (d) (10 points) Use ggibiplot to visualize the rst two principal components. Provide a concise in
#terpretation of the results.

library(ggbiplot)
ggbiplot(
  Hpca,
  obs.scale = 1,
  var.scale = 1,
  groups = rownames(heptathlon_clean),
  ellipse = TRUE,
  circle = TRUE,
  var.axes = TRUE
) +
  theme_minimal() +
  ggtitle("Biplot of the First Two Principal Components") +
  labs(x = "PC1", y = "PC2", color = "Competitors") +  # Set consistent legend title
  theme(legend.position = "bottom") +
  guides(fill = "none") +  # Remove the redundant legend for fill
  scale_color_discrete(name = "Competitors")  # Ensure a single legend for Competitors


cat("PCA Scores for the first two components (PC1 and PC2):\n")
print(Hpca$x[, 1:2])  # Scores of PC1 and PC2 for each competitor

cat("\nPCA Loadings (Rotations) on PC1 and PC2:\n")
print(Hpca$rotation[, 1:2])  # Loadings of PC1 and PC2 for each event


#-------------------------------------------------------------------------------------------------

# Question 2 (e) (10 points) The PCA projections onto principal components 123
# for each competitor can now
# be accessed as Hpca$x[,1], Hpca$x[,2], Hpca$x[,3], .... Plot the heptathlon score against the
# principal component 1 projections. Brie y discuss these results.

# Extract the heptathlon scores and the first principal component projections
heptathlon_scores <- heptathlon_clean$score
pc1_projections <- Hpca$x[, 1]

# Plot the heptathlon scores against the PC1 projections
plot(
  pc1_projections,
  heptathlon_scores,
  xlab = "Principal Component 1 (PC1) Projections",
  ylab = "Heptathlon Score",
  main = "Heptathlon Score vs. PC1 Projections",
  pch = 19,
  col = "blue"
)

# Add a linear regression line to visualize the trend
abline(lm(heptathlon_scores ~ pc1_projections), col = "red")


#Supporting Analysis Code for Interpretation:

# Calculate and print the correlation coefficient between PC1 projections and heptathlon scores
correlation <- cor(pc1_projections, heptathlon_scores)
cat("Correlation between PC1 Projections and Heptathlon Scores:",
    correlation,
    "\n")

lm_model <- lm(heptathlon_scores ~ pc1_projections)
cat("Linear Regression Coefficients:\n")
print(summary(lm_model)$coefficients)

cat("\nSummary of PC1 Projections:\n")
print(summary(pc1_projections))

cat("\nSummary of Heptathlon Scores:\n")
print(summary(heptathlon_scores))



#-------------------------------------------------------------------------------------------------
# Question 3 Housing data dimension reduction and exploration

# Load housing data into a data frame
housingData <- read.csv("C:/Users/aqsa/Desktop/DSA/Assignment 3/housingData.csv")
head(housingData)

# Transform the data
hd <- housingData %>%
  select_if(is.numeric) %>%
  dplyr::mutate(
    age = YrSold - YearBuilt,
    ageSinceRemodel = YrSold - YearRemodAdd,
    ageofGarage = ifelse(is.na(GarageYrBlt), YrSold - YearBuilt, YrSold - GarageYrBlt)
  ) %>%
  dplyr::select(
    !c(
      Id,
      MSSubClass,
      LotFrontage,
      GarageYrBlt,
      MiscVal,
      YrSold,
      MoSold,
      YearBuilt,
      YearRemodAdd,
      MasVnrArea
    )
  )



# Standardize the numeric columns in the hd dataset
hd_scaled <- scale(hd)

# Perform PCA
pca_result <- prcomp(hd_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA result
summary(pca_result)



# Plot the PCA results using ggbiplot
ggbiplot(
  pca_result,
  obs.scale = 1,
  var.scale = 1,
  groups = NULL,
  ellipse = TRUE,
  circle = TRUE
) +
  theme_minimal() +
  ggtitle("PCA Biplot of Housing Data")

# Compute the correlation matrix
correlation_matrix <- cor(hd, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", tl.cex = 0.6)


# Supporting Analysis Code for Interpretation:

# Find and display the top 10 most positively and negatively correlated variable pairs
cor_matrix <- correlation_matrix
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
cor_df <- as.data.frame(as.table(cor_matrix))
cor_df <- cor_df[!is.na(cor_df$Freq), ]
top_correlated_pairs <- cor_df[order(-abs(cor_df$Freq)), ][1:10, ]

# Print the top 10 correlated pairs
print(top_correlated_pairs)


#-------------------------------------------------------------------------------------------------
