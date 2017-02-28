#Biome Transition script

library(maptools)
library(maps)
library(mapproj)
library(rgeos)
library(rgdal)
library(sp)
library(BIEN)

#Save an object containing the coordinate reference system of WGS 84
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
#Read in biome shape files
ecos<-readShapePoly("C:/Users/Cecina/Desktop/HypervolumeFiles/Terrestrial_Ecoregions/wwf_terr_ecos.shp",proj4string = P4S.latlon)

#List of biomes and their areas
area=NULL
for(i in 1:14) {
  area[i]<-sum(ecos@data$AREA[ecos@data$BIOME==i])
}
area
#The smallest biome is biome 14 (mangroves)
#second smallest is biome 3 (subtropical and tropical coniferous forests)

#Extract list of species within a given biome (doesn't currently work)
mangrovespecies<-BIEN_ranges_shapefile(ecos[ecos@data$BIOME=="14",])

#runs the command and records the time to run
system.time(mangrovespecies<-BIEN_ranges_shapefile(ecos[ecos@data$BIOME=="14",]))

#want to check if occurrences are all that different


#import dataset from Brian Maitner
#rasterized all BIEN range maps
#counted any cell as an occurrence if the range map covered at least 1% of the cell
bien_8_25_2016_100km_1percent_occurrence_only <- read_csv("C:/Users/Cecina/Desktop/plants/bien_8_25_2016_100km_1percent_occurrence_only.csv")
View(bien_8_25_2016_100km_1percent_occurrence_only)




