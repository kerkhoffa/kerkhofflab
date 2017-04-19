#Biome Transition script

library(maptools)
library(maps)
library(mapproj)
library(rgeos)
library(rgdal)
library(sp)
library(BIEN)
library(mosaic)

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

plot(biome_transform[biome_transform@data$BIOME=="6",],add=T,col="blue")
plot(biome_transform[biome_transform@data$BIOME=="1",],add=T,col="green")

#List of biomes and their areas
area=NULL
for(i in 1:14) {
  area[i]<-sum(ecos@data$AREA[ecos@data$BIOME==i])
}
area
#The smallest biome is biome 14 (mangroves)
#second smallest is biome 3 (subtropical and tropical coniferous forests)

#Create list of cells occupied by biomes
biome_cells<-data.frame(Biome=character(),occupied_cells=character(),stringsAsFactors = FALSE)
for(i in 1:length(unique(biome_transform@data$BIOME))) {
  biome_i<-biome_transform[biome_transform@data$BIOME==unique(biome_transform@data$BIOME)[i],]
  biome_id<-unique(biome_transform@data$BIOME)[i]
  biomeraster_i<-rasterize(biome_i,emptyraster)
  biome_cells_i<-Which(biomeraster_i>0,cells=TRUE)
  biomedata_i<-data.frame(Biome=rep(biome_id,length(biome_cells_i)),occupied_cells=biome_cells_i)
  biome_cells<-rbind(biomedata_i,biome_cells)
}

colnames(biome_cells)<-c("Biome","occupied_cells")

# #Add biome numbers to species list
# bien_speciesrange_cells<-bien_8_25_2016_100km_1percent_occurrence_only
# bien_speciesrange_cells$biome<-NA
# for(i in 1:length(bien_speciesrange_cells$biome)){
#   try(bien_speciesrange_cells$biome[i]<-biome_cells$Biome[which(biome_cells$occupied_cells==bien_speciesrange_cells$occupied_cells[i])])
# }
# #finished sometime after 9400...

#Looping on biomes
for(i in 1:14){
  biome_cells_i<-biome_cells$occupied_cells[which(biome_cells$Biome==i)]
  bien_speciesrange_cells$biome[which(is.element(bien_speciesrange_cells$occupied_cells,biome_cells_i))]<-i
}



#Create raster with values equal to the cell numbers
testraster<-setValues(emptyraster,1:ncell(emptyraster))
plot(testraster)

#Create raster with values equal to the biome numbers
biomeraster_test<-emptyraster
for(i in 1:14){
  biomeraster_test[biome_cells$occupied_cells[which(biome_cells$Biome==i)]]<-i
}

plot(biomeraster_test)
plot(biomeraster_test,col=rainbow(14))
plot(biomeraster_test,col=colorRampPalette(14))

quercus_bicolor_test<-emptyraster
quercus_bicolor_test[bien_speciesrange_cells$occupied_cells[which(bien_speciesrange_cells$current_species=="Quercus_bicolor")]]<-20
plot(quercus_bicolor_test,add=TRUE) #overlays range of Quercus bicolor (makes the scale look weird)


#tally number of cells for each species in each biome
speciesbybiome<-tally(current_species~biome, data=bien_speciesrange_cells)
speciesbybiome<-as.data.frame(speciesbybiome)


#Find maximum for each species
#Divide by sum of frequencies
#look in tidyr or dplyr to summarize by a variable


#Example using just the sugar maple
sugarmaple<-subset(as.data.frame(speciesbybiome),current_species=="Acer_saccharum")
#Finding the biome of highest frequency for the sugar maple
maplebiome<-sugarmaple %>% group_by(current_species) %>%
  + summarize(freqbiome=biome[which(Freq==max(Freq))],highfreq=max(Freq),totalcells=sum(Freq))

#Example using sugar maple and white oak
whiteoak_sugarmaple<-subset(as.data.frame(speciesbybiome),(current_species=="Acer_saccharum")|(current_species=="Quercus_alba"))
#Finding the biome of highest frequency for the sugar maple
oak_maplebiome<-whiteoak_sugarmaple %>% group_by(current_species) %>%
  summarize(freqbiome=biome[which(Freq==max(Freq))],highfreq=max(Freq),totalcells=sum(Freq))


#Now for the whole data set!
#need to deal with situations with multiple maxima

#This doesn't work because there are ties
species_biome_frequencies<-as.data.frame(speciesbybiome) %>% group_by(current_species) %>%
  summarize(freqbiome=biome[which(Freq==max(Freq))],maxfreq=max(Freq),totalcells=sum(Freq))

#This works great but doesn't have the total frequencies
species_biome_frequencies<-as.data.frame(speciesbybiome) %>% group_by(current_species) %>%
  filter(Freq==max(Freq))
#Would want to add a column for the sum of all frequencies
  
#This adds the biomes tied for the maximum pasted in with commas (maybe not ideal but it works)
species_biome_frequencies1<-as.data.frame(speciesbybiome) %>% group_by(current_species) %>%
  summarize(freqbiome = paste(biome[which(Freq == max(Freq))], collapse = ", "),maxfreq=max(Freq),totalcells=sum(Freq))
#Need to account for situations in which "NA" is the most frequent biome
