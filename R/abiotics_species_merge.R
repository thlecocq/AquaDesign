#' Title Merge species and abiotics dataframes by matching coordinates
#'
#' @param abiotics_df data frame with abiotics data
#' @param species_df data frame with species occurrences
#'
#' @return Returns a dataframe of abiotic data matching the species coordinates
#' @export
#'
#' @examples

abiotics_species_merge <- function(abiotics_df, species_df) {

  #round abiotics to have homogeneous coordinates
  is.num <- sapply(abiotics_df, is.numeric)
  abiotics_df[is.num] <- lapply(abiotics_df[is.num], round, 4)

  #round species to have homogeneous coordinates
  is.num <- sapply(species_df, is.numeric)
  species_df[is.num] <- lapply(species_df[is.num], round, 4)

  #get species list
  species_list <- unique(species_df$species)

  #merge and crop datasets to obtain columns as : Species / var 1 / var 2 / ...
  species_abiotics_df <- data.frame(matrix(ncol = length(names(abiotics_df)), nrow = 0))
  column_names <- names(abiotics_df)[3:length(names(abiotics_df))]
  colnames(species_abiotics_df) <- c("species", column_names)

  #set a progress bar
  pb <- progress_bar$new(total = length(species_list))
  pb$tick(0)

  for (i in 1:length(species_list)){

    #update the progress bar
    if (!pb$finished==TRUE){
      pb$update(i/length(species_list))
    }

    species <- species_df[species_df$species==unique(species_df$species)[i],]
    species_name <- merge(species, abiotics_df, by=c("x","y")) #merge species i with factors data frame
    colnames(species_name)[3] <- c("species")                  #rename "species name" column
    species_name$species[species_name$species==1] <- as.character(unique(species_df$species)[i]) #replace cell with value = 1 with the species name
    species_name <-na.omit(species_name)                                #remove NA
    species_name <- species_name[,-(1:2)]                               #remove x and y columns
    species_abiotics_df <-rbind(species_abiotics_df, species_name)
  }

  pb$terminate()

  return(species_abiotics_df)
}
