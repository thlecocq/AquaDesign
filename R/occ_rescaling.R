#' Title Rescale species coordinates and/or abiotic coordinates at 10 or 30 arcmins
#'
#' @param data_cl species coordinates and/or abiotic coordinates dataframe
#' @param resolution Integer; Chosen resolution (10 or 30, default 30)
#'
#'@import raster
#'@import progress
#'@import sf
#' @return Returns a rescaled coordinates dataframe
#' @export
#'
#' @examples
occ_rescaling <- function(data_cl, resolution=30){

  #Coordinates=occurences, hence the name of this function
  
  #create empty dataframe
  species_df<- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(species_df) <- c("x", "y", "species")

  #convert data_frame in sf object (spatial object)
  point<-st_as_sf(data_cl, coords = c("decimalLongitude", "decimalLatitude"), crs ="WGS84")

  # Create an empty raster (vals = 0) at 30 arcmins (res = 0.5)  in WGS84 (crs(point))
  if (resolution == 30){
    rast <- raster(crs = crs(point), vals=0,resolution = c(0.5, 0.5))
    # Rasterization (Presence map) and stocking the results in a multi-layers raster brick
    brick_rast <- brick(nrows=nrow(rast),ncol=ncol(rast),nl=length(unique(point$species))) #nl = create as many layers as different species
  }

  if (resolution == 10){
    rast <- raster(crs = crs(point), vals=0,resolution = c(1/6, 1/6))
    # Rasterization (Presence map) and stocking the results in a multi-layers raster brick
    brick_rast <- brick(nrows=nrow(rast),ncol=ncol(rast),nl=length(unique(point$species))) #nl = create as many layers as different species
  }

  #set a progress bar
  pb <- progress_bar$new(total = length(unique(point$species)))
  pb$tick(0)

  #for each species, rasterize the occurence points in 30 arcmins rasterbricks and get a dataframe
  for(i in 1:length(unique(point$species))){

    #update progress bar
    if (!pb$finished==TRUE){
      pb$update(i/length(unique(point$species)))
    }

    point.i<-point[point$species==unique(point$species)[i],] #point.i is a "sf" dataframe with all the occurence points of species i
    brick_rast[[i]]<-rasterize(as(point.i, "Spatial"),rast,field=1)
    df <- as.data.frame(brick_rast[[i]], xy=TRUE, centroids = TRUE)
    colnames(df)[3] <- c("species")
    df$species[df$species==1] <- as.character(unique(point$species)[i]) #if the species is present(=1) replace "1" with species name
    df <-na.omit(df) #remove NA
    species_df <- rbind(species_df, df) #add to the global dataframe
  }

  #round species coordinates to prepare merge with abiotic data
  is.num <- sapply(species_df, is.numeric)
  species_df[is.num] <- lapply(species_df[is.num], round,
                              4)
  #end progress bar
  pb$terminate()

  return(species_df)
}
