# ----------------------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# An introduction to multivariate analysis
# First version 2022-08-05
# ----------------------------------------------------------------#

# Import libraies
library(palmerpenguins)
library(vegan)
library(cluster)
library(dplyr)
# Load the dataset----
data(dune)
data(dune.env)
table(dune.env$Management)

# Euclidean more than 1.
bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

# Cluster analysis
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

# Plot the dendrograms
par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))

# Improve the plots
par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1, 1))

# Use PCA in the data
norm <- decostand(dune, "norm")
pca <- rda(norm)
plot(pca)

# Plot the PCA in the components 2 and 3
plot(pca, choices = c(2, 3))
# PCA of environmental
#All variables are character
dune1 <- dune.env
dune1$A1 <- as.numeric(dune.env$A1)
dune1$Moisture <- as.numeric(dune.env$Moisture)
dune1$Manure <- as.numeric(dune.env$Manure)
pca_env <- rda(dune1[, c("A1", "Moisture", "Manure")])
plot(pca_env)
