#BIEN3 EXAMPLES

#Examples:BIEN.gis.species
BIEN.gis.species("Abies amabilis")
species_vector<-c("Abies amabilis", "Acer nigrum")
BIEN.gis.species(species_vector)


#Examples: BIEN.list.country

BIEN.list.country("Canada")
country_vector<-c("Canada","United States")
BIEN.list.country(country_vector)

#Examples: BIEN.list.state
BIEN.list.state("Michigan")
state_vector<-c("Michigan","Arizona")
BIEN.list.state(state_vector)

#Examples: BIEN.list.county
BIEN.list.county("Michigan","Kent")
BIEN.list.county(state="Michigan", county="Kent")
county_vector<-c("Kent","Kalamazoo")
BIEN.list.county(state="Michigan",county=county_vector)#Need to test this code

#Examples: BIEN.gis.genus

BIEN.gis.genus("Abutilon")
genus_vector<-c("Abutilon","Abronia")
BIEN.gis.genus(genus_vector)


BIEN.gis.genus(genus = "Abutilon",cultivated = TRUE,only.new.world = FALSE)#returns all records for the genus Abutilon, including cultivates and Old World records.



#Examples: BIEN.gis.family
BIEN.gis.family("Theaceae")
family_vector<-c("Theaceae","Ericaceae")
BIEN.gis.family(family_vector)

#Examples: BIEN.gis.state
BIEN.gis.state("Rhode Island")
state_vector<-c("Rhode Island","Maryland")
BIEN.gis.state(state_vector)

#Examples: BIEN.gis.box
output_test<-BIEN.gis.box(min.lat = 32,max.lat = 33,min.long = -114,max.long = -113,cultivated = TRUE, only.new.world = FALSE)


#Examples:BIEN.trait.species
BIEN.trait.species("Poa annua")
species_vector<-c("Poa annua","Juncus trifidus")
BIEN.trait.species(species_vector)

#Examples: BIEN.trait.trait
BIEN.trait.trait("Height")
trait_vector<-c("Height", "Leaf dry mass")
BIEN.trait.trait(trait_vector)

#Examples: BIEN.trait.traitbyspecies
BIEN.trait.traitbyspecies(trait = "Height", species = "Carex capitata")
trait_vector<-c("Height", "Leaf dry mass")
species_vector<-c("Carex capitata","Betula nana")
BIEN.trait.traitbyspecies(trait=trait_vector,species=species_vector)

#Examples: BIEN.trait.list
BIEN.trait.list()

#Examples: BIEN.ranges.species
species_vector<-c("Abies_lasiocarpa","Abies_amabilis")
testwd<-"C:/wherever/you/want/files/saved/" #Set a working directory,Obviously change this to suit your needs
BIEN.ranges.species(species_vector)
BIEN.ranges.species(species_vector,test_wd)#saves ranges to a specified working directory
BIEN.ranges.species("Abies_lasiocarpa")
BIEN.ranges.species("Abies_lasiocarpa","C:/wherever/you/want/files/saved/")

#Reading files
setwd("C:/wherever/your/shapefiles/are/")
Abies_poly<-readShapePoly("Abies_lasiocarpa")#you will need to make sure to set your working directory or to specify the full filepath
Abies_poly<-readShapePoly("C:/wherever/your/shapefiles/are/Abies_lasiocarpa.shp")#again, you'll need to change this code as well

#Plotting files
plot(Abies_poly)#plots the shapefile, but doesn't mean much without any reference 
require(maps)#easy source of maps
map('world', xlim = c(-175,-35), ylim = c(-70,90), fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in forest grey
plot(Abies_poly, col="forest green",add=TRUE) #adds the range of Abies lasiocarpa to our world map

#Getting data from the files (currently only species names)
Abies_poly$Species#gives the species name associated with "Abies_poly"

