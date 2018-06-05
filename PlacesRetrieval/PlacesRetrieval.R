## Author: Miguel Gautier
## Created on June 2018



## Name of this project

Name <- "PlacesRetrieval"


# Start of the Script 

## This script aims to retrieve from Google Places API places 
## that comply with the specified location, radius, keyword, type, and keyword.
## The default configuration will set the radius in 50 000 meters (maximum allowed),
## and for the location a table will be provided, with the coordinates necessary to 
## search through all the Dominican territory. 
## Type and keyword can be changed in the script. 
## In a updated version the script will ask the user to provide these variables.
## Also will allow to choose a specific province or region within the DR to search. 

#Packages 

    ##XML package
    if(!("XML" %in% rownames(installed.packages()))){
      install.packages("XML")
    }
    library("XML")
  
    ##data.table Package
    if(!("data.table" %in% rownames(installed.packages()))){
      install.packages("data.table")
    }

    library(data.table)


#Variables

    ## Search radius
    radius <- 50000   
    radius <- file.path("radius",radius,fsep = "=")
    
    ## Keyword that identifies the place searched. 
    keyword <- "popular"
    keyword <- file.path("keyword",keyword,fsep = "=")
    
    ## Type of place searched. To see available types 
    ## typesvalues <- read.table("Type.txt",header = FALSE)
    type <- "bank"
    type <- file.path("type",type,fsep = "=")
    
    ## API key 
    key <- "AIzaSyBaUM6Q8PciqtZQSkgJ_Oa1Pdwq_xJCzcg"
    key <- file.path("key",key,fsep = "=")
    
    ## Group variables in a single string
    variables <- file.path(radius,type,keyword,key,fsep = "&")
    
    
    ## Search points.
    ##txt file that contains coordinates of search points around the DR
    locationstable <- data.frame(read.table("Coordenadas.txt", stringsAsFactors = FALSE))
    names(locationstable) <- c("Coordinates") 
    lim <- nrow(locationstable)
    
    ## Function to generate the correspondent url string for each coordinate
    url <- function(i){
      location <- file.path("location",locationstable[i,1],fsep = "=")
      return(file.path(file.path("https://maps.googleapis.com/maps/api/place/radarsearch/xml?",
                location,fsep = ""),variables,fsep = "&"))
    }
    
    ## Inserting url values and an ID to each coordinate as additional columns
    locationstable$URL<- url(seq(1,lim))
    locationstable$ID <- seq(1,lim) 
  
    ## Loop Coordinates to get locations 
    coordinates <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("lat", "lng"))
    
    for (i in 1:lim) {
      tree <- read_xml(locationstable$URL[i])
      latitude<-setNames(as.data.frame(xml_text(xml_find_all(tree,"//lat"))),"lat")
      longitude<-setNames(as.data.frame(xml_text(xml_find_all(tree,"//lng"))),"lng")
      coordinates <- rbind(coordinates,cbind.data.frame(latitude,longitude))
      print(locationstable$ID[i])
    }
    ## Write coordinates into a csv file
    write.csv(coordinates,file = "Coordenadas")