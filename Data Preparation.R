# Load necessary libraries
library(ggplot2)  # For data visualization
library(MASS)     # For boxcox transformation
library(mice)     # For handling missing data
library(dplyr)    # For data manipulation
library(forcats)  # For factor handling
library(tidyverse)
library(readr)
library(moments)

housingData <- read.csv("housingData-1.csv")
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)


housingNumeric <- housingData %>%
  dplyr::select(where(is.numeric))

housingFactor <- housingData %>%
  transmute(across(where(is.character), as.factor))

glimpse(housingNumeric)
glimpse(housingFactor)

Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}
# Apply myNumericSummary to every numeric variable and reshape it properly
numericSummary <- housingNumeric %>%
  summarise(across(everything(), myNumericSummary)) 


# Bind stat labels
numericSummary <- cbind(
  stat = c("n", "unique", "missing", "mean", "min", "Q1", "median", "Q3", "max", "sd"),
  numericSummary
)

# View the summary
glimpse(numericSummary)

numericSummaryFinal <- numericSummary %>%
  pivot_longer("Id":"ageofGarage", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
library(knitr)
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()
# Write the table to a CSV file
write.csv(numericSummaryFinal, "numeric_summary_final.csv", row.names = FALSE)


# Function to generate a summary for categorical variables
myCategoricalSummary <- function(x) {
  # Frequency table
  tbl <- table(x, useNA = "ifany")
  sorted_tbl <- sort(tbl, decreasing = TRUE)
  
  # First mode and frequency
  first_mode <- names(sorted_tbl[1])
  first_mode_freq <- as.integer(sorted_tbl[1])
  
  # Second mode and frequency (if available)
  second_mode <- ifelse(length(sorted_tbl) > 1, names(sorted_tbl[2]), NA)
  second_mode_freq <- ifelse(length(sorted_tbl) > 1, as.integer(sorted_tbl[2]), NA)
  
  # Least common value and frequency
  least_common <- names(sorted_tbl[length(sorted_tbl)])
  least_common_freq <- as.integer(sorted_tbl[length(sorted_tbl)])
  
  # Summary
  c(
    n = length(x),
    missing = sum(is.na(x)),
    missing_pct = 100 * sum(is.na(x)) / length(x),
    unique = n_distinct(x),
    unique_pct = 100 * n_distinct(x) / length(x),
    freq_ratio = first_mode_freq / length(x),
    first_mode = first_mode,
    first_mode_freq = first_mode_freq,
    second_mode = second_mode,
    second_mode_freq = second_mode_freq,
    least_common = least_common,
    least_common_freq = least_common_freq
  )
}

# Apply myCategoricalSummary to every categorical variable
categoricalSummary <- housingFactor %>%
  summarise(across(everything(), myCategoricalSummary))

# Reshape the data to match the desired format
categoricalSummary <- cbind(
  stat = c("n", "missing", "missing_pct", "unique", "unique_pct", "freq_ratio",
           "first_mode", "first_mode_freq", "second_mode", "second_mode_freq", 
           "least_common", "least_common_freq"),
  categoricalSummary
)

# Reshape to long format
categoricalSummary <- categoricalSummary %>%
  pivot_longer(cols = -stat, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(c(n, missing, missing_pct, unique, unique_pct, 
                  first_mode_freq, second_mode_freq, least_common_freq), as.numeric))

# View the summary
glimpse(categoricalSummary)
library(knitr)
options(digits=3)
options(scipen=99)

# Display the final table with knitr::kable
categoricalSummary %>% kable()
# Write the table to a CSV file
write.csv(categoricalSummary, "categorical_summary_final.csv", row.names = FALSE)








### 2(a) Identify Highly Skewed Numeric Variables and Apply Transformations

# Select numeric variables to check for skewness
numeric_columns = housingData %>% select(where(is.numeric))

# Function to plot histograms for all numeric variables to visually inspect skewness
plot_histograms = function(data) {
  for (col_name in names(data)) {
    # Create histogram for each numeric variable
    p = ggplot(data, aes_string(x = col_name)) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      ggtitle(paste("Histogram of", col_name)) +
      theme_minimal()
    print(p)
  }
}


# Run the function to generate histograms for all numeric variables
plot_histograms(numeric_columns)


# Apply log transformation to reduce skewness (for right-skewed data) and create new data
housingData$log_GrLivArea = log(housingData$GrLivArea + 1)  # Adding 1 to avoid log(0)
housingData$log_BsmtUnfSF = log(housingData$BsmtUnfSF + 1)

# new histrogram plot for Grlivearea and Bsmtunfsf
ggplot(housingData, aes(x = log_GrLivArea)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogram of GrLivArea") +
  theme_minimal() +
  xlab("GrLivArea") +
  ylab("Frequency")


ggplot(housingData, aes(x = log_BsmtUnfSF)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogram of BsmtUnfSF") +
  theme_minimal() +
  xlab("BsmtUnfSF") +
  ylab("count")


### 2(b) Imputation of Missing Values for `LotFrontage`

# i. Mean Imputation for LotFrontage
housingData$LotFrontage_meanImputed = ifelse(is.na(housingData$LotFrontage),
mean(housingData$LotFrontage, na.rm = TRUE),housingData$LotFrontage)

print(housingData$LotFrontage_meanImputed)

# ii. Regression with Error Imputation
# Using linear regression to predict LotFrontage based on other variables
reg_model = lm(LotFrontage ~ LotArea + YearBuilt, data = housingData, na.action = na.exclude)
housingData$LotFrontage_regImputed = predict(reg_model, newdata = housingData)
# Add random error (residuals from the regression model) to imputed values
housingData$LotFrontage_regImputed = ifelse(is.na(housingData$LotFrontage),
housingData$LotFrontage_regImputed + rnorm(nrow(housingData), 
0, summary(reg_model)$sigma), housingData$LotFrontage)

print(housingData$LotFrontage_regImputed)

# iii. Predictive Mean Matching Imputation (Using mice package)
# Set up the mice method for imputing missing values using PMM
mice_data = mice(housingData, m = 5, method = 'pmm', maxit = 5)
housingData_complete = complete(mice_data)  # Complete data with imputed values for LotFrontage

print(mice_data)

# iv. Visualize imputed data distributions (Histogram for each method and original)
ggplot(housingData, aes(x = LotFrontage)) +  geom_histogram(bins = 30) + 
  ggtitle("Histogram of LotFrontage") +
  theme_minimal() +  xlab("LotFrontage") +  ylab("count")

ggplot(housingData, aes(x = LotFrontage_meanImputed)) + 
  geom_histogram(bins = 30) + ggtitle("Mean Imputed LotFrontage")

ggplot(housingData, aes(x = LotFrontage_regImputed)) +
  geom_histogram(bins = 30) + ggtitle("Regression Imputed LotFrontage")

ggplot(housingData_complete, aes(x = LotFrontage)) + 
  geom_histogram(bins = 30) + ggtitle("PMM Imputed LotFrontage")

### 2(c) Collapse Factor Levels in Exterior1st with forecat

# Collapse levels of Exterior1st into the top 4 most frequent levels, with others in other
housingData$Exterior1st_collapsed = fct_lump_n(housingData$Exterior1st, n = 4)

# Display the new levels
table(housingData$Exterior1st_collapsed)

### 2(d) Average SalePrice by Neighborhood and Reorder Levels

# i. average SalePrice by Neighborhood
average_saleprice = housingData %>%
  group_by(Neighborhood) %>%
  summarise(avg_saleprice = mean(SalePrice, na.rm = TRUE))

# ii. Create a parallel boxplot of SalePrice by Neighborhood
ggplot(housingData, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Prices by Neighborhood") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# iii. Re-order the Neighborhood factor levels based on median SalePrice
housingData$Neighborhood = fct_reorder(housingData$Neighborhood,
housingData$SalePrice,.fun = median,.desc = TRUE)

table(housingData$Neighborhood)

# iv. Create the parallel boxplot with reordered Neighborhoods
ggplot(housingData, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Sale Prices by Reordered Neighborhoods") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
