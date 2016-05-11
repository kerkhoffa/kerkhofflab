#This code queries BIEN3 for species range polygons then plots them on the 
#world such that each range has a unique color.

#required packages
library(maps)#easy source of maps

#set up vector and get data
species_vector <- c("Abies_amabilis", "Acer_pseudoplatanus")
len <- length(species_vector)
BIEN.ranges.species(species_vector)

#set up color series 
color_vector <- rainbow(len)

#Just americas
#plots a world map (WGS84 projection), in forest grey
map('world', xlim = c(-175,-35), ylim = c(-70,90), fill = TRUE, col = "grey")

#for each species, plot it onto map in different color
for(i in 1:len) {
  #you will need to make sure to set your working directory or to specify the full filepath
  species_poly<-readShapePoly(species_vector[i])
  #alpha in the the scales package
  plot(species_poly,col=alpha(color_vector[i], 0.3),add=TRUE)
}
