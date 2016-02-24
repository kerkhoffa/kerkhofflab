#Geographic Phytolaccoid Data using climate variables
###Examples using Rivina humilis
#####################################################

#Extracting Geographic Data
#source BIEN code
source('/Volumes/lloydg$/Senior Year/Research-Kerkhoff Lab/R Scripts/BIEN_API_FILE_Compilation.R', echo=TRUE)

#set up species_vector
family_vector<-c("Phytolaccaceae", "Sarcobataceae", "Nyctaginaceae")
family_loc_data <- BIEN.gis.family(family_vector)
species_vector = unique(family_loc_data[2])

#BIEN.ranges.species
#load package rgeos, rgdal, maptools, and maps
#saveto <- "/Users/lloydg/Documents/Kerkhoff Lab/Phytolaccoids/Shape Files"
#setwd(saveto)

BIEN.ranges.species(species_vector,saveto)
BIEN.ranges.species("Rivina humilis", saveto)

#Reading files
Rivina_poly <- readShapePoly("Rivina_humilis")
Rivina_poly <- readShapePoly("/Users/lloydg/Documents/Kerkhoff Lab/Phytolaccoids/Shape Files/Rivina_humilis.shp") #this works hollaaaaa

#################################################
#################################################
#Plotting files
map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in forest grey
plot(Rivina_poly,col="forest green",add=TRUE)

#####################
#plotting Rivina humilis on Google Maps alone (points)
#load package Rgooglemaps, stats, scales, graphics

df <- data.frame(latitude=double(), longitude=double(), color=character(), name=character())

for (i in 1:length(Rivina_vector)) {
  Rivina_vector <- BIEN.gis.species("Rivina humilis")
  tmp <- Rivina_vector[2:3]
  tmp = na.omit(tmp)
  tmp$color <- alpha("red", 0.3)
  temp_dataframe = data.frame(tmp[1], tmp[2])
  df <- rbind(df, temp_dataframe)
}

lon_range <- c(min(df$longitude), max(df$longitude))
lat_range <- c(min(df$latitude), max(df$latitude))

mymap <- GetMap.bbox(lon_range, lat_range, destfile = "TestGoogleMap.png", maptype="satellite", zoom=2, size=c(640,640))
PlotOnStaticMap(mymap, lon=df$longitude, lat=df$latitude, pch=20, cex = .25, col="red")

#mypolygon <- drawPoly()  # click on the map to draw a polygon and press ESC when finished
#summary(mypolygon) 


##########
#plotting Rivina humilis ranges on Google Maps

#create map using ggmap
#importing shape files into a usable r function
Rivina_shp <- readOGR(".", "Rivina_humilis")
projection(Rivina_shp) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs")

#load ggplot2, ggmap
ggplot(data = Rivina_shp, aes(x = long, y = lat, group = group)) + geom_polygon()
CenterOfMap <- geocode("Mexico")
mymap = get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), source="google", maptype="satellite", zoom=2)
ggmap(mymap)+ geom_polygon(aes(x = long, y = lat, group = group), data = Rivina_shp, fill='green')

#############################################
#############################################
#BIOCLIM DATA
####bioclim1 (annual mean temp)

#load raster, sp, stats, utils
#in order to access the raster both the .bil and .hdr files need to be in the working directory

#move working directory
saveto <- "/Users/lloydg/Documents/Kerkhoff Lab/Phytolaccoids/BioClim"
setwd(saveto)

bio1= raster("bio_1.bil")
#fromDisk(bio1)
bio1 <-bio1/10

#create gis_data
gis_data = Rivina_vector[2:3]
gis_data = unique(gis_data)
gis_data = na.omit(gis_data)

str(gis_data)
coordinates(gis_data) <- ~longitude+latitude
proj4string(gis_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#plotting bioclim data with species range
plot(bio1)
points(gis_data, col = "red", pch = 20, cex = 0.25)

bio1_location_data = extract(bio1, gis_data)

#summary(bio1_location_data)
#hist(bio1_location_data)

##############################################
#####using raster data with the trait data####

Rivina_shp <- readOGR(".", "Rivina_humilis")
projection(Rivina_shp) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs")

#plotting bioclim data with species range
plot(bio1)
geom_polygon(aes(x = long, y = lat, group = group), data = Rivina_shp, fill='green')

bio1_location_data = extract(bio1, gis_data)

#######################DO THIS SO THAT IT WORKS PLZZZZ###################################
##Stacking rasters aka lots of bioclim variables on top of each other

#########
#worldclim tho
bios <-getData ("worldclim", var = "bio", res = 2.5) 

#this works!!!
species_bio <- extract (bios, Rivina_poly) 

#this does means instead (it works #blessed #ijustdidahappydancearoundthelab)
range_bio <- extract(bios,Rivina_poly, fun='mean', na.rm=TRUE) 
