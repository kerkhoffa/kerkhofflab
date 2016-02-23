#This code will produce a data set from a family or family vector
#then convert this to a list of species that can be used for other searches. 

#required packages
library(stats)

#gets location data about each family sorted by speices
family_vector <- c("Phytolaccaceae", "Sarcobataceae", "Nyctaginaceae")
family_loc_data <- BIEN.gis.family(family_vector)

#take species name column and remove redundancies
species_vector = family_loc_data[2]
species_vector = unique(species_vector)
species_vector = na.omit(species_vector)

#gets traits specified by trait vector and searches for them for each species
trait_vector <- c("Height", "Leaf dry mass")
trait_data <- BIEN.trait.traitbyspecies(trait = trait_vector, species = species_vector[,1])
