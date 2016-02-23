

#####################
#Lat/Long from Species Names(s)
#species here can be either a single species or a vector of species
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true

BIEN.gis.species<-function(species,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  # set the query
  occQuery <- paste("SELECT scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}


###############################
#Species list from country/countries
#Accepts single countries or vectors
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true

BIEN.list.country<-function(country,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  
  
  
  
  # set the query
  occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only
  
  
  
  query = occQuery
  
  #print(query)
  
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}





############################
#Species list from state/province
#Accepts single countries or vectors
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true

BIEN.list.state<-function(state,cultivated=FALSE,only.new.world=TRUE){
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  
  # set the query
  occQuery <- paste("SELECT DISTINCT state_province, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")" ,paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY state_province,scrubbed_species_binomial;")
  
  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
###########################
#Species list from a given county (and state)
#Requires both state and county be supplied
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true
#Although both state and county can be supplied as a vector, I recommend sticking to one state at a time unless you're certain there are no overlapping county names between the states.


BIEN.list.county<-function(state,county,cultivated=FALSE,only.new.world=TRUE){
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  
  # set the query
  occQuery <- paste("SELECT DISTINCT state_province, county, scrubbed_species_binomial",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ") AND  county in (", paste(shQuote(county, type = "sh"),collapse = ', '), ") " ,paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY state_province,county,scrubbed_species_binomial;")
  
  #occQuery <- paste("SELECT DISTINCT country, scrubbed_species_binomial FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ") AND (is_cultivated = 0 OR is_cultivated IS NULL) AND is_new_world = 1 AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial LIMIT 2;") #Limit for testing only
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}

###########################
#BIEN.list.all

#Lists all species in the BIEN database

BIEN.list.all<-function(){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  
  
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  occQuery <- paste("SELECT DISTINCT scrubbed_species_binomial FROM view_full_occurrence_individual ORDER BY scrubbed_species_binomial;")
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  dbDisconnect(con)
  return(df)
  
}


###########################
#Occurrences from Genus
#Accepts a single Genus or a vector of Genera
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true



BIEN.gis.genus<-function(genus,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  
  
  
  
  # set the query
  occQuery <- paste("SELECT scrubbed_genus, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select), "FROM view_full_occurrence_individual WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}

############################
#Occurrences from Family
#Accepts a single Family or a vector of Families
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true



BIEN.gis.family<-function(family,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  
  
  
  
  # set the query
  occQuery <- paste("SELECT scrubbed_family, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}


#######################
#Occurrences from State/Province
#Accepts a single State or a vector
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true.  Probably not very useful to change at the moment.
#Insanely slow

BIEN.gis.state<-function(state,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
    # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  # set the query
  occQuery <- paste("SELECT state_province, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE state_province in (", paste(shQuote(state, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
#############################
#BIEN.gis.country
#Occurrences from Country
#Accepts a single Country or a vector
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true.  Probably not very useful to change at the moment.
#Insanely slow

BIEN.gis.country<-function(country,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
    # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  # set the query
  occQuery <- paste("SELECT country, scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE country in (", paste(shQuote(country, type = "sh"),collapse = ', '), ")",paste(cultivated_query,newworld_query)," AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY country,scrubbed_species_binomial;")
  
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}




##############################


#Occurrence records from from lat/long Bounding box
#Will only do one box at a time, specified by min/max lat and long
#cultivated= Return cultivated records as well?  Default is FALSE.
#only.new.world = Return only records from the New World?  Default is true

BIEN.gis.box<-function(min.lat,max.lat,min.long,max.long,cultivated=FALSE,only.new.world=TRUE){
  
  library("RPostgreSQL")
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  #set conditions for query
  
  if(cultivated == FALSE){
    cultivated_query<-"AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select<-""
  }
  
  if(cultivated==TRUE){
    cultivated_query<-""  
    cultivated_select<-",is_cultivated"
  }
  
  if(only.new.world==TRUE){
    newworld_query<-"AND is_new_world = 1 "  
    newworld_select<-""  
  }
  
  if(only.new.world==FALSE){
    newworld_query<-""  
    newworld_select<-",is_new_world"
    
  }
  
  
  # set the query
  occQuery <- paste("SELECT scrubbed_species_binomial, latitude, longitude,date_collected,datasource,custodial_institution_codes,collection_code",paste(cultivated_select,newworld_select),"FROM view_full_occurrence_individual WHERE latitude between " , paste(shQuote(min.lat, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.lat, type = "sh"),collapse = ', '),"AND longitude between ", paste(shQuote(min.long, type = "sh"),collapse = ', '), "AND " , paste(shQuote(max.long, type = "sh"),collapse = ', '), paste(cultivated_query,newworld_query),  "AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}





###
#BIEN ranges function
#inputs
#species= species as either a single species or a vector of species
#directory = the directory where you want the range shapefiles saved.  Default is your current working directory
#matched = returns information on whether range maps were found for each species submitted
#match_names_only = Do you want to check whether the maps you're looking for exist without downloading them?

BIEN.ranges.species<-function(species,directory=NULL,matched=TRUE,match_names_only=FALSE){
  library(RPostgreSQL)
  library(rgeos)
  library(rgdal)
  library(maptools)
  host='vegbiendev.nceas.ucsb.edu'
  user='public_bien'
  password='bien_public'
    #Specify the dbname for range maps
  dbname='public_vegbien'
  
  #record original working directory,change to specified directory if given
  wd<-getwd()
  if(is.null(directory)==FALSE){
    setwd(directory) 
  }

  
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  #make sure there are no spaces in the species names
  species<-gsub(" ","_",species)
  
  if(match_names_only==FALSE){
    # set the query
    rangeQuery <- paste("SELECT ST_AsText(geom),species,gid FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    #rangeQuery <- paste("SELECT * FROM ranges LIMIT 10;")
    #rangeQuery <- paste("SELECT species FROM ranges LIMIT 100000;")

    
    
    
    query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- dbGetQuery(con, statement = query);
    
    dbDisconnect(con)
    
    if(length(df)==0){
      message("No species matched")  
    }else{
      
      for(l in 1:length(df$species)){
        Species<-df$species[l] 
        #sp_range<-readWKT(df$st_astext[l])  
        sp_range<-readWKT(df$st_astext[l],p4s="+init=epsg:3857")  
        #proj4string(sp_range) <- CRS("+init=epsg:3857")
        sp_range<-spTransform(sp_range,CRS("+init=epsg:4326"))
        #assign(paste(species),sp_range,envir=.GlobalEnv)   
        
        #convert shapepoly into a spatialpolygon dataframe(needed to save as a shapefile)
        spdf<-as.data.frame(Species)
        spdf<-SpatialPolygonsDataFrame(sp_range,spdf)
        class(spdf)
        writePolyShape(x=spdf,fn = Species)
        #save output
        
      }#for species in df loop
    }#else
    
    setwd(wd) #return wd to original 
    
    #list matched species
    if(matched==TRUE){
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))  
      colnames(found)<-c("Species","Range_map_downloaded?")
      found$`Range_map_downloaded?`<-as.character(found$`Range_map_downloaded?`)
      found$`Range_map_downloaded?`[which(species%in%df$species)]<-"Yes"
      return(found)
    }#matched = true
  }#match names only if statement  
  
  if(match_names_only==TRUE){
    
    rangeQuery <- paste("SELECT species FROM ranges WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species;")
    query = rangeQuery
    #print(query)
    # create query to retrieve
    df <- dbGetQuery(con, statement = query);
    
    dbDisconnect(con)
    
    if(length(df)==0){
      message("No species matched")  
    }else{
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))  
      colnames(found)<-c("Species","Range_map_available?")
      found$`Range_map_available?`<-as.character(found$`Range_map_available?`)
      found$`Range_map_available?`[which(species%in%df$species)]<-"Yes"
      return(found)
      
    }
    
  } #matched_names_only ==TRUE 
  
  
  
}

############################




#####################

#Traits from species
#Accepts a single species or a vector


BIEN.trait.species<-function(species){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}


############################
#Traits from trait name
#Accepts a single trait or a vector


BIEN.trait.trait<-function(trait){
  library("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") ORDER BY taxon;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
############################
#This function downloads trait data for given species

BIEN.trait.traitbyspecies<-function(trait,species){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND taxon in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY taxon,trait_name;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
###########################
#This function downloads specific trait data for given genus/genera

BIEN.trait.traitbygenus<-function(trait,genus){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus,taxon,trait_name;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
###########################
#This function downloads specific trait data for given family/families

BIEN.trait.traitbyfamily<-function(trait,family){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE trait_name in (", paste(shQuote(trait, type = "sh"),collapse = ', '), ") AND family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon,trait_name;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}
############################
#Traits from genus/genera
#Accepts a single genus or a vector


BIEN.trait.genus<-function(genus){
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  library("RPostgreSQL")
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE genus in (", paste(shQuote(genus, type = "sh"),collapse = ', '), ") ORDER BY genus;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}

###########################

#Traits from family/families
#Accepts a single family or a vector


BIEN.trait.family<-function(family){
  library("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT * FROM agg_traits WHERE family in (", paste(shQuote(family, type = "sh"),collapse = ', '), ") ORDER BY family,taxon;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}





############################
#This function lists all currently available types of trait data.
#This is especially useful if you want to figure out what your trait of interest is titled in this database.

BIEN.trait.list<-function(){
  library("RPostgreSQL")
  # Load in the current connection info
  host='vegbiendev.nceas.ucsb.edu'
  dbname='public_vegbien'
  user='public_bien'
  password='bien_public'
  # Name the database type that will be used
  drv <- dbDriver('PostgreSQL')
  # establish connection with database
  con <- dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  # set the query
  occQuery <- paste("SELECT DISTINCT trait_name FROM agg_traits ORDER BY trait_name;")
  
  
  
  query = occQuery
  #print(query)
  # create query to retrieve
  df <- dbGetQuery(con, statement = query);
  
  
  
  
  dbDisconnect(con)
  return(df)
  
  
}

