#This code will plot a locations of species in a family or family vector.
#This is the same basic proceedure that you need to plot a shape polygon.
#The code to do so is commented out.

#required packages
library(stats)
library(maps)  #easy source of maps
library(graphics)

#gets location data about each family sorted by speices
family_vector <- c("Phytolaccaceae", "Sarcobataceae", "Nyctaginaceae")
family_loc_data <- BIEN.gis.family(family_vector)
#generic species range
#species_vector <- c("Abies_lasiocarpa","Abies_amabilis")
#BIEN.ranges.species(species_vector)

loc_data <- family_loc_data[3:4]
loc_data = na.omit(loc_data)

#plots a world map (WGS84 projection), in forest grey
map('world', xlim = c(-175,-35), ylim = c(-70,90), fill = TRUE, col = "grey")

#Now use the code written for plotting multiple polygons on the same map to plot any
#polygons you want to place on the map.

points(loc_data$longitude, loc_data$latitude, pch=19, col="red", cex=0.25)

