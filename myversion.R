# ===============================
# 1. Load required libraries
# ===============================
library(Factoshiny)
library(FactoMineR)
library(dplyr)

# ===============================
# 2. Load Adult dataset
# ===============================
data <- read.csv("adult.csv", stringsAsFactors = FALSE)

cat("Original column names:\n")
print(names(data))

# ===============================
# 3. Data preparation
# ===============================

# Select only the variables for MCA
data_selected <- data %>%
  select(
    age,
    workclass,
    education,
    marital.status,
    occupation,
    relationship,
    sex,
    income
  )

# Convert qualitative variables to factors
data_selected$workclass      <- as.factor(data_selected$workclass)
data_selected$education      <- as.factor(data_selected$education)
data_selected$marital.status <- as.factor(data_selected$marital.status)
data_selected$occupation     <- as.factor(data_selected$occupation)
data_selected$relationship   <- as.factor(data_selected$relationship)
data_selected$sex            <- as.factor(data_selected$sex)
data_selected$income         <- as.factor(data_selected$income)

# Quantitative variable
data_selected$age <- as.numeric(data_selected$age)

# Remove rows with missing values
data_selected <- na.omit(data_selected)

# ===============================
# 4. Check prepared data
# ===============================
cat("\nPrepared data structure:\n")
str(data_selected)

cat("\nNumber of individuals:", nrow(data_selected), "\n")
cat("Number of variables:", ncol(data_selected), "\n")

cat("\nModalities per qualitative variable:\n")
for (var in names(data_selected)) {
  if (is.factor(data_selected[[var]])) {
    cat(var, ":", length(levels(data_selected[[var]])), "modalities\n")
  } else {
    cat(var, ": quantitative\n")
  }
}

# ===============================
# 5. Sampling (recommended)
# ===============================
set.seed(123)

# Take a reasonable sample (Adult is large)
data_sample <- data_selected[sample(nrow(data_selected), 600), ]

cat("\nSample size:", nrow(data_sample), "\n")

# ===============================
# 6. Launch Factoshiny
# ===============================
cat("\n=== LAUNCHING FACTOSHINY ===\n")
Factoshiny(data_sample)