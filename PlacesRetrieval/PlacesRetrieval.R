## Author: Miguel Gautier
## Created on June 2018



## Name of this project

Name <- "PlacesRetrieval"


# Start of the Script 

## This script aims to retrieve from Google Places API places 
## that comply with the specified location, radius, keyword, type, and keyword.


#Packages 

    ##XML package
    if(!("xml2" %in% rownames(installed.packages()))){
      install.packages("xml2")
    }
    library("xml2" )
  
    ##data.table Package
    if(!("data.table" %in% rownames(installed.packages()))){
      install.packages("data.table")
    }

    library(data.table)


#Variables

    ## Search radius
    if (!exists("radius")){
      radius <- 50000   
    }
    radius <- file.path("radius",radius,fsep = "=")
    
    ## API key 
    if (exists("key")){
      key <- file.path("key",key,fsep = "=")
    } else print("Key must be specified")
    
    ## Keyword that identifies the place searched. 
    if (exists("keyword")){
      keyword <- file.path("keyword",keyword,fsep = "=")  
    } 
    
    
    ## Type of place searched. To see available types 
    ## typesvalues <- read.table("Type.txt",header = FALSE)
    if (exists("type")){
      type <- file.path("type",type,fsep = "=")
    }
    
   
    ## Group variables in a single string
    if (exists("keyword") && exists("type")){
      variables <- file.path(radius,type,keyword,key,fsep = "&")
    }
    if (!exists("keyword") && exists("type")){
      variables <- file.path(radius,type,key,fsep = "&")
    }
    if (exists("keyword") && !exists("type")){
      variables <- file.path(radius,keyword,key,fsep = "&")
    }
    
    ## Search points.
    ##txt file that contains coordinates of search points around the DR
    if(!exists(location)){
      locationstable <- data.frame(read.table("Coordenadas.txt", stringsAsFactors = FALSE))
    } else locationstable <- data.frame(location)
    
    names(locationstable) <- c("Coordinates") 
    lim <- nrow(locationstable)
    
    ## Function to generate the correspondent url string for each coordinate
    url1 <- function(i){
      location <- file.path("location",locationstable[i,1],fsep = "=")
      return(file.path(file.path("https://maps.googleapis.com/maps/api/place/radarsearch/xml?",
                location,fsep = ""),variables,fsep = "&"))
    }
    
    ## Inserting url values and an ID to each coordinate as additional columns
    locationstable$URL<- url1(seq(1,lim))
    locationstable$ID <- seq(1,lim) 
  
    ## Loop Coordinates to get locations 
  
    coordinates <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("lat", "lng"))
    
    for (i in 1:lim) {
      radar <- read_xml(locationstable$URL[i])
      latitude<-setNames(as.data.frame(xml_text(xml_find_all(radar,"//lat"))),"lat")
      longitude<-setNames(as.data.frame(xml_text(xml_find_all(radar,"//lng"))),"lng")
      placeid <- setNames(as.data.frame(xml_text(xml_find_all(radar,"//place_id"))),"Place ID")
      coordinates <- rbind(coordinates,cbind.data.frame(latitude,longitude,placeid))
      
      print(locationstable$ID[i])
    }
    
    ## Function to generate the correspondent url string for each placeid
    url2 <- function(i){
      PlaceId <- file.path("placeid",coordinates$`Place ID`[i],fsep = "=")
      return(file.path(file.path("https://maps.googleapis.com/maps/api/place/details/xml?",
                                 PlaceId,fsep = ""),key,fsep = "&"))
    }
    ## Inserting url values and an ID to each coordinate as additional columns
    P <- nrow(coordinates)
    placesidURL<- url2(seq(1,P))
    placesdetails <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Name", "Phone", "Address"))
    
    ## Loop Places ID to get details 
    
    for (i in 1:P) {
      details <- read_xml(placesidURL[i])
      
      placename<-xml_text(xml_find_all(details,"//name"))
      placeaddress<- xml_text(xml_find_all(details,"//formatted_address"))
      placephone<-if(identical(xml_text(xml_find_all(details,"//formatted_phone_number"))
                     ,character(0))){
        "ND"
      } else xml_text(xml_find_all(details,"//formatted_phone_number"))
      
      placesdetails <- rbind(placesdetails,cbind(placename,placephone, placeaddress))
      print(i)
      
  }
 
    ## Write coordinates into a csv file
    write.csv(coordinates,file = "Coordenadas.csv")
    write.csv(placesdetails, file = "Detalles.csv")
    remove(key,keyword,type,variables)