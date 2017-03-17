#Biome Transition script

library(maptools)
library(maps)
library(mapproj)
library(rgeos)
library(rgdal)
library(sp)
library(BIEN)

#import dataset from Brian Maitner
#rasterized all BIEN range maps
#counted any cell as an occurrence if the range map covered at least 1% of the cell
bien_8_25_2016_100km_1percent_occurrence_only <- read_csv("C:/Users/Cecina/Desktop/plants/bien_8_25_2016_100km_1percent_occurrence_only.csv")
View(bien_8_25_2016_100km_1percent_occurrence_only)



#import WWF biome maps
biome<-readOGR(dsn = "C:/Users/Cecina/Desktop/HypervolumeFiles/Terrestrial_Ecoregions",layer ="wwf_terr_ecos" )
#For shapefiles, the dsn is basically the folder containing the shapefile components
#Note that there is no "/" at the end of the dsn
#The field layer is basically the filename (without the .shp or any of the other stuff)
#readOGR is a bit more confusing to use, but automatically imports the projections if they exist, making things easier

biome@proj4string

#Rasterize the biome according to the raster of the occurrence list
emptyraster<-raster("blank_100km_raster.tif")
#Get projection of the raster
crs(emptyraster)

#transform biomes to same projection as the raster
biome_transform<-spTransform(x = biome,CRSobj = emptyraster@crs)

plot(emptyraster)
plot(biome_transform,add=T)



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






