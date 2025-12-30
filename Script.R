# =========================
# TP Analyse Factorielle (ACM)
# Dataset : shopping_behavior.csv (sans Customer ID)
# =========================

# -------------------------
# Étape 0 : Installer les packages nécessaires
# -------------------------
install.packages("FactoMineR", dependencies = TRUE)
install.packages("factoextra")
install.packages("dplyr")   # pour manipulations
install.packages("ggplot2") # pour graphiques
install.packages("ade4")    # pour tableau de Burt

library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(ade4)

# -------------------------
# Étape 1 : Charger le dataset
# -------------------------
data <- read.csv("shopping_behavior.csv", stringsAsFactors = TRUE)

# Aperçu
dim(data)
str(data)
summary(data)

# -------------------------
# Étape 2 : Analyse descriptive
# -------------------------

# 2.1 Variable quantitative : Purchase Amount
quant_var <- data$Purchase.Amount..USD.

cat("Moyenne :", mean(quant_var), "\n")
cat("Médiane :", median(quant_var), "\n")
cat("Min :", min(quant_var), "\n")
cat("Max :", max(quant_var), "\n")
cat("Écart-type :", sd(quant_var), "\n")

# Histogramme
hist(quant_var, main="Histogram of Purchase Amount", 
     xlab="Purchase Amount (USD)", col="skyblue")

# 2.2 Variables qualitatives
qual_vars <- c("Gender", "Item.Purchased", "Category", "Location", 
               "Size", "Color", "Season", "Subscription.Status", 
               "Shipping.Type", "Discount.Applied", "Promo.Code.Used", 
               "Payment.Method", "Frequency.of.Purchases")

# Tableaux de fréquences
for (v in qual_vars) {
  cat("\nTableau de fréquences pour :", v, "\n")
  print(table(data[[v]]))
  
  # Barplot
  barplot(table(data[[v]]), main=paste("Barplot of", v), col="lightgreen", las=2)
}

# -------------------------
# Étape 3 : Transformer les variables qualitatives (TDC / One-hot)
# -------------------------
# FactoMineR gère les facteurs, mais si tu veux un tableau disjonctif :
data_qual <- data[, qual_vars]
tdc <- acm.disjonctif(data_qual)

# Vérification des marges
rowSums(tdc)  # doit être égal au nombre de variables qualitatives
head(tdc)

save(data, quant_var, qual_vars, tdc, file="data_prepared.RData")

