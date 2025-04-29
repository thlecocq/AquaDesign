#' Title Remove points from species coordinates outside specified range or if they are suspicious
#'
#' @param data Dataframe: A data frame downloaded from GBIF
#' @param minlat Float; Minimum latitude (optional)
#' @param maxlat Float; Maximum latitude (optional)
#' @param minlong Float; Minimum longitude (optional)
#' @param maxlong Float; Maximum longitude (optional)
#' @param check.out Boolean; Check outliers (default=FALSE, takes a long time for a large number of occurences)
#' @param filtercapitals Boolean; Remove coordinates equal to countries capitals (default=TRUE)
#' @param filtercountrycentroids Boolean; Remove coordinates equal to countries centroids (default=TRUE)
#' @param filtercountrymismatches Boolean; Remove coordinates if the coordinates aren't in the country corresponding to countryCode (default=FALSE, takes a long time)
#' @param filtergbifheadquarters Boolean; Remove coordinates equal to the GBIF headquarters (default=TRUE)
#' @param filterinstitutions Boolean; Remove coordinates equal to the world institutions (default=TRUE)
#' @param filtersea Boolean; Remove coordinates in seas and oceans (default=FALSE, takes a long time)
#'
#'@importFrom dplyr select
#'@importFrom dplyr filter
#'@import countrycode
#'@import CoordinateCleaner
#'@import rnaturalearthdata
#' @return Returns a cleaned species coordinates dataframe
#' @export
#'
#' @examples
data_cleaning <- function(data, minlat=NA, maxlat=NA, minlong=NA, maxlong=NA, check.out = FALSE, filtercapitals = TRUE, filtercountrycentroids = TRUE, filtercountrymismatches = FALSE, filtergbifheadquarters=TRUE, filterinstitutions=TRUE, filtersea = FALSE){
  #The filtering of country mismatches (cc_coun) is at least 30 minutes long (it didn't end after 30min), the other functions are mere seconds, there is probably a problem with it, so I'm removing it
  #The filtering of marine life (cc_sea) is also long
  #As a result, I'm desactivating this filters by default in data_cleaning arguments
  
  #keep only the columns of interest and set spatial extent
  #data <- data%>%
  #  dplyr::select(species, decimalLongitude, decimalLatitude, gbifID, countryCode)%>%
  #  filter(decimalLatitude<maxlat)%>%
  #  filter(decimalLatitude>(minlat))%>%
  #  filter(decimalLongitude<maxlong)%>%
  #  filter(decimalLongitude>(minlong))

  #keep only the columns of interest : it's already done in the gbif csv reading now
  #data <- data %>% dplyr::select(species, decimalLongitude, decimalLatitude, gbifID, countryCode)

  #facultative coordinates filtering: the filtering should be already done in the download but the user could still do it here
  #it allows the library to still be compatible with old aquadesign scripts, just in case the user didnt update the script
  if (!is.na(maxlat)){
    data <- data%>%filter(decimalLatitude<maxlat)
  }
  
  if (!is.na(minlat)){
    data <- data%>%filter(decimalLatitude>minlat)
  }
      
  if (!is.na(maxlong)){
    data <- data%>%filter(decimalLatitude<maxlong)
  }
      
  if (!is.na(minlong)){
    data <- data%>%filter(decimalLatitude<minlong)
  }
      
  #prepare data for cleaning
  #an update to CoordinateCleaner passed the default column names "decimallongitude" and "decimallatitude" to "decimalLongitude" and "decimalLatitude", respectively, breaking the functions calls
  #hence, I'm removing the column renaming, keeping the correct ones
  #names(data)[2:3] <- c("decimallongitude", "decimallatitude") #these are default names for latitude and longitude in the following functions

  data$countryCode <-  countrycode(data$countryCode, origin =  'iso2c', destination = 'iso3c') #iso 2 --> iso 3 changes countrycode from 2 letters to 3 letters (ex : FR --> FRA) to be able to use cc_count()

  #country code puts Na for countrycodes not matched unambiguously (XK and ZZ = Kosovo and undefined countries), remove the Na
  data <- na.omit(data)

  #removes suspicious points (buffer = range in meters)

  #in all cases remove invalid values
  data <- data%>% cc_val()
  if (filtercapitals){
    data <- data%>% cc_cap(buffer=10000)
  }
  if (filtercountrycentroids){
    data <- data%>% cc_cen(buffer = 1000)
  }
  if (filtercountrymismatches){
    data <- data%>% cc_coun(iso3 = "countryCode")
  }
  if (filtergbifheadquarters){
    data <- data%>% cc_gbif(buffer=1000)
  }
  if (filterinstitutions){
    data <- data%>% cc_inst(buffer=100)
  }
  if (filtersea){
    data <- data%>% cc_sea()
  }
  
  #data <- data%>%
  #  cc_val()%>%  #invalid values
  #  cc_cap(buffer=10000)%>%   #capitals
  #  cc_cen(buffer = 1000)%>%  #country centroids
  #  cc_coun(iso3 = "countryCode")%>%  #country mismatches
  #  cc_gbif(buffer=1000)%>%  #gbif HeadQuarters
  #  cc_inst(buffer=100)%>%  #institutions
  #  cc_sea()  #sea
  
  if (check.out == TRUE){
    #check outliers
    data <- data%>%   #/!\ takes quite a long time /!\
      cc_outl(
        species = "species", #check species by species
        method = "distance",
        tdi = 1000,          #if a point is more than 1000km away from every other points from the same species, it is removed
        value = "clean",
        thinning = TRUE,
        thinning_res = 0.5
      )
  }

  #Renaming back latitude and longitude is obsolete since the columns stays at decimalLongitude and decimalLatitude from the start
  #names(data)[2:3] <- c("decimalLongitude", "decimalLatitude")

  return(data)
}
