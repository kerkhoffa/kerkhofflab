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

#want to check if occurrences are all that different


#Old occurrence list code:
#occurrence list file of the BIEN species and the biomes in which they occur
BIENoccurrence_list<-NULL
for(i in 1:length(BIENtrait3taxa$Species)){
  print(i)
  shpfile_i<-paste("C:/Users/Cecina/Desktop/plants/shpfiles/",BIENtrait3taxa$Species[i],".shp",sep="")
  
  if(file.exists(shpfile_i)){
    BIENranges_i<-readShapePoly(shpfile_i,proj4string = P4S.latlon)
    BIENecos_i<-ecos[BIENranges_i,]#Note this line of code. This way of subsetting GIS data is a REALLY powerful trick to know
    BIENspecies<-BIENtrait3taxa$Species[i]  
    if(length(BIENecos_i)>0){
      BIENoccurrence_list<-rbind(BIENoccurrence_list,cbind(BIENspecies,unique(BIENecos_i@data[,4:18])))
    }#if
    
  }#if
}#file.exists