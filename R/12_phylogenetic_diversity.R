# From taxonomical to functional and phylogenetic diversity in R ----
# Importing packages
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(Taxonstand)
library(taxize)
library(dplyr)

# Reading the data
comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")
splist <- read.csv("data/raw/cestes/splist.csv")

# Maninupating and organizing data
head(comm[,1:6])
rownames(comm)[1:6]
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

head(traits)[,1:6]
rownames(traits) <- traits[,1]
traits <- traits[,-1]
head(traits)[,1:6]

# Species Richness ----

richness <- vegan::specnumber(comm)

shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

# Distance matrix between individuals, where individuals of the same species
# have a distance of zero between them, individuals of different species have a
# distance of 1 between them
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?
class(gow) #different classes ha
class(gow2)
plot(gow, gow2, asp = 1) # have the same values but given that the classes are
# not the same it is not identical

tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)

plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)

#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)

#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)

#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)

plot(FuncDiv$FDis, FuncDiv1$FDiv)

# Check the species' family

classification_data <- classification(splist$TaxonName, db = "ncbi")


tible_ex <- classification_data[[1]] %>%
  filter(rank == "family") %>%
  select(name)

extract_family <- function(x){
  if(!is.null(dim(x))){
    y <- x %>%
      filter(rank == "family") %>%
      pull(name)
    return(y)
  }
}

families <- vector()
for(i in 1:length(classification_data)){
  f <- extract_family(classification_data[[i]])
  if(length(f) > 0) families[i] <- f
}

splist$Family <-  families
