# =========================
# TP Analyse Factorielle (ACM)
# Dataset : shopping_behavior_updated.csv
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
data <- read.csv("shopping_behavior_updated.csv", stringsAsFactors = TRUE)

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

# Tableau de Burt (facultatif)
burt_tab <- Burt(tdc)
head(burt_tab)

# -------------------------
# Étape 4 : Analyse factorielle des correspondances multiples (ACM)
# -------------------------
acm_res <- MCA(data_qual, graph = FALSE)

# Résultats
acm_res$eig  # valeurs propres, inertie
acm_res$ind$coord  # coordonnées des individus
acm_res$ind$contrib # contributions des individus
acm_res$ind$cos2    # qualité de représentation des individus

acm_res$var$coord   # coordonnées des modalités
acm_res$var$contrib # contributions des modalités
acm_res$var$cos2    # qualité de représentation des modalités

# η² pour les variables
acm_res$var$eta2

# -------------------------
# Étape 5 : Visualisation
# -------------------------
# Individus
fviz_mca_ind(acm_res, 
             repel = TRUE, 
             col.ind = "cos2",
             gradient.cols = c("blue", "orange", "red"))

# Modalités
fviz_mca_var(acm_res, 
             repel = TRUE, 
             col.var = "contrib",
             gradient.cols = c("blue", "orange", "red"))

# Variables qualitatives
fviz_mca_var(acm_res, choice="eta2", 
             repel = TRUE,
             col.var="cos2",
             gradient.cols=c("purple","green","yellow"))

# -------------------------
# Étape 6 : Interprétation (à compléter dans ton rapport)
# -------------------------
# Exemple de guide d’interprétation :
# - Examiner les axes factoriels : quelles variables contribuent le plus à chaque axe
# - Identifier les modalités importantes
# - Observer la proximité entre individus → profils similaires
# - Observer la proximité entre modalités → associations entre catégories
# - Décrire les groupements ou tendances
