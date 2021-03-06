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
bien_8_25_2016_100km_1percent_occurrence_only <- read.csv("C:/Users/Cecina/Desktop/plants/bien_8_25_2016_100km_1percent_occurrence_only.csv")
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

#biome 98 is the Great Lakes, biome 99 is Greenland

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

#adjusting the legend
par(mar=c(2,2.5,2,25))
plot(biomeraster_test,col=rainbow(14),legend = FALSE,xlim=c(-5000200,5000200))
legend("topright",inset=c(-1.7,0),legend=biomenames[1:14],fill=rainbow(14),xpd=TRUE)

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

#This gets rid of all of the NAs
species_biome_frequencies<-speciesbybiome[which(!is.na(speciesbybiome$biome)),] %>% group_by(current_species) %>%
  summarize(freqbiome = paste(biome[which(Freq == max(Freq))], collapse = ", "),maxfreq=max(Freq),totalcells=sum(Freq))
#get rid of the species that occur in 0 cells
species_biome_frequencies<-species_biome_frequencies[which(species_biome_frequencies$totalcells!=0)]
#Add column for the percentage of total cells that fall within the biome of highest frequency
species_biome_frequencies$percentage<-species_biome_frequencies$maxfreq/species_biome_frequencies$totalcells

#Create a histogram of the percentage of total cells that fall within the biome of highest frequency
histogram(species_biome_frequencies$percentage,xlab="Percent of Cells in Majority Biome", ylab="Percentage of Species")
histogram(na.omit(species_biome_frequencies$percentage),xlab="Percent of Cells in Majority Biome", ylab="Percentage of Species")
hist(na.omit(species_biome_frequencies$percentage),xlab="Percent of Cells in Majority Biome", prob=FALSE,col="seagreen",main="",ylab="Number of Species")

#get rid of all of the ones that are 100%
histogram(species_biome_frequencies$percentage,xlim=c(0,0.98),ylim=c(0,2))

#2729 species occur in 0 cells
length(which(species_biome_frequencies$maxfreq==0))
#remove all species with maximum frequency of 0
species_biome_frequencies<-species_biome_frequencies[which(species_biome_frequencies$totalcells!=0),]


#7278 species exist in multiple biomes
length(grep(",", species_biome_frequencies$freqbiome))

#extract these species with ties
species_biome_frequencies_tied<-species_biome_frequencies[grep(",", species_biome_frequencies$freqbiome),]
#split up the column freqbiome by the commas
split<-strsplit(species_biome_frequencies_tied$freqbiome, split=",")
species_biome_frequencies_tied<-data.frame(current_species = rep(species_biome_frequencies_tied$current_species, sapply(split, length)), freqbiome = unlist(split), maxfreq = rep(species_biome_frequencies_tied$maxfreq,sapply(split,length)),totalcells = rep(species_biome_frequencies_tied$totalcells,sapply(split,length)))

#take out the ties in the original dataframe
species_biome_frequencies_noties<-species_biome_frequencies[grep(",",species_biome_frequencies$freqbiome,invert = TRUE),]

#bind the ties and not ties into one frequency list
total_species_biome_frequencies<-rbind(species_biome_frequencies_tied,species_biome_frequencies_noties)
write.csv(total_species_biome_frequencies,"total_species_biome_frequencies.csv")
total_species_biome_frequencies <- read_csv("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/kerkhofflab/Biome-Transitions/total_species_biome_frequencies.csv")
#unclear why I have to do this but then life works yay!


#Want a list of species with max occurrence in each biome
species_biome_list<-data.frame(Biome=character(),species=character(),stringsAsFactors = FALSE)
#adding all species (tied and not tied)
for(i in 1:14) {
  print(i)
  rows_i<-total_species_biome_frequencies[which(total_species_biome_frequencies$freqbiome==i),]
  species_list_i<-total_species_biome_frequencies$current_species[which(total_species_biome_frequencies$freqbiome==i)]
  species_biome_data_i<-data.frame(Biome=rep(i, length(length(species_list_i))),species=species_list_i)
  species_biome_list<-rbind(species_biome_data_i,species_biome_list)
}


#species richness by biome
biome_richness<-data.frame(Biome=1:14,richness=numeric(length = 14))
for(i in 1:14){
  biome_richness$richness[i]<-length(species_biome_list$Biome[which(species_biome_list$Biome==i)])
}

plot(biome_richness$richness~biome_richness$Biome)



#boxplot of richness in each biome
#for species that aren't necessarily in the biome

biome_cells$speciesrichness<-NA
for(i in 1:length(biome_cells$Biome)){
  biome_cells$speciesrichness[i]<-length(which(bien_speciesrange_cells$occupied_cells==biome_cells$occupied_cells[i]))
}

#boxplot with colors matching the raster plot from earlier
biomenames<-c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
              "Temperate Grasslands, Savannas, & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves","","")
boxplot(biome_cells$speciesrichness~biome_cells$Biome, xlim=c(1,14),col=rainbow(14),ylab="Species Richness per Cell",names = biomenames)

par(mar=c(14, 4.1, 0.53, 2.1))
boxplot(biome_cells$speciesrichness~biome_cells$Biome, xlim=c(1,14),col=rainbow(14),ylab="Species Richness per Cell",xaxt="n")
#axis(1,at = 1:14,labels = FALSE)
text(x =  seq_along(biomenames), y = par("usr")[3] - 1, srt = 65, adj = 1,
          labels = biomenames, xpd = TRUE,cex=0.7)

