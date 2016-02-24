#This code gets family location coordinates from BIEN3 and then plots them
#onto a map produced in Google Maps. The points are color-coded by family.

#required packages
library(RgoogleMaps)
library(stats)
library(scales)
library(graphics)

#get GIS coords for a family vector
family_vector <-c ("Nyctaginaceae","Phytolaccaceae", "Sarcobataceae")
#family_vector <-c ("Sarcobataceae")

#makes a blank data frame to be the combination of all valid gis coordinates
df <- data.frame(latitude=double(), longitude=double(), color=character(), name=character())

#makes a set of non-redundant colors
color_vector <- rainbow(length(family_vector))

#for every family pull out the locations, remove na values, assign each point a color based on family
for (i in 1:length(family_vector)) {
  family_loc_data <- BIEN.gis.family(family_vector[i])
  tmp <- family_loc_data[3:4]
  tmp = na.omit(tmp)
  tmp$name <- family_vector[i]
  #need to enable scales package to use alpha
  tmp$color <- alpha(color_vector[i], 0.3)
  
  #builds onto dataframe
  temp_dataframe = data.frame(tmp[1], tmp[2], tmp[3], tmp[4])
  df <- rbind(df, temp_dataframe)
}

#get bounds of plot
lon_range <- c(min(df$longitude), max(df$longitude))
lat_range <- c(min(df$latitude), max(df$latitude))

#make plot and then plot each data gis point onto map
#afterwards, makes a legend on the map
mymap <- GetMap.bbox(lon_range, lat_range, destfile = "TestGoogleMap.png", maptype="satellite", zoom=2, size=c(640,640))
PlotOnStaticMap(mymap, lon=df$longitude, lat=df$latitude, pch=20, cex = .25, col=df$color)
legend("bottomleft", legend = unique(family_vector), fill = color_vector, title = "Legend")

