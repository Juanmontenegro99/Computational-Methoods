# Reading the data
comm <- read.csv("data/raw/cestes/comm.csv")
dim(comm)
head(comm[,1:6])
#Ejercicio 1
# Encontrar las 5 especies más abundantes
five_most_abundant <- sort(colSums(comm), decreasing = TRUE)[2:6]
# Ejercicio 2
# Encontrar la abundancia de cada sitio
sites_abundant <- data.frame("Species" = (comm[,1]), "Abundance" = rowSums(comm[,2:ncol(comm)]>0))
# EJercicio 3
# Encontrar la especie más abundante en cada sitio
res <- list()
for(i in 1:nrow(comm)-1){
  res <-  append(res, names(sort(comm[i+1,2:ncol(comm)], decreasing = TRUE)[1]))
}
species_most_abundant <- data.frame("Site" = 1:97, "Specie" = array(unlist(res)))

# Shannon diversity index
my_shannon <- function(x){
  pi <- x/sum(x)
  H <- sum(pi*log(pi))
}

# Simpson diversity index
my_simpson <- function(x){
  pi <- x/sum(x)
  Simp <- 1-sum(pi^2)
}

# Inverse simpson diversity index
inverse_simpson <- function(x){
  pi <- x/sum(x)
  Simp <- 1/sum(pi^2)
}

# Clase del 3 de agosto
Community.A <- c(10, 6, 4, 1)
Community.B <- c(17, rep(1, 7))

# Librería de community ecology
library("vegan")
diversity(Community.A, "shannon")
diversity(Community.B, "shannon")
diversity(Community.A, "invsimpso")
diversity(Community.B, "invsimpso")

# Shannon es sensible al tamaño de la población mientras que el simpson es más
# robusto y muestra cual es más diverso

ren_comA <- renyi(Community.A)
ren_comB <- renyi(Community.B)

# Hill es el que te permite observar en 1 el exp(shannon) y 2 el invsimpson
ren_comA_H <- renyi(Community.A, hill = T)
ren_comB_H <- renyi(Community.B, hill = T)

# Crear una matriz
renAB <- rbind(ren_comA, ren_comB)

matplot(t(renAB), type = 'l', axes = F, ylab = "Diversity", xlab = "a", lwd = 3)
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright", legend = c("Community A", "Community B"), lty = c(1, 2),
       col = c("black", "red"))
