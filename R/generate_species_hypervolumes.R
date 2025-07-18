#' Title Generate a list of hypervolumes from a list of species
#'
#' @param species_list_selected List of the selected species
#' @param rescaled_abiotics Selected and rescaled abiotics pca axis in a data frame
#' @param skip_check Boolean; By default the program won't work if there are not enough occurences of a species. You can skip this check (log(length(data[[1]]))>(length(rescaled_abiotics)-1)) by turning this flag to TRUE (default=FALSE)
#'
#'@import hypervolume
#' @return Returns hypervolumes list
#' @export
#'
#' @examples
generate_species_hv <- function(species_list, rescaled_abiotics,skip_check=FALSE){

  hv_list <- c() #a list to be filled with all the species hypervolumes

  for (i in 1:length(species_list)){
    data <- subset(rescaled_abiotics, species==species_list[i])[,2:length(rescaled_abiotics)]
    progression=paste0("Hypervolumes generation... ",i,"/",length(species_list)," : ",species_list[i])
    print(progression)
    if(!skip_check){
      if (log(length(data[[1]]))>(length(rescaled_abiotics)-1)){  #if there are not enough occurrences for some species, a warning appear
        invisible(capture.output({hv_species <- hypervolume(data, method = "svm")})) #generate hypervolume with single vector method
        hv_species@Name <- species_list[[i]] #set hypervolume name
        hv_list<- hypervolume_join(hv_list, hv_species) #add the hypervolume to the list
      }
      else {
        warning(paste0(species_list[i], " does not have enough values( ",length(data[[1]]), ") to be studied and has been removed from the list, please remove them from species_list and rerun it"))
      }
    }
    if(skip_check){
      invisible(capture.output({hv_species <- hypervolume(data, method = "svm")})) #generate hypervolume with single vector method
      hv_species@Name <- species_list[[i]] #set hypervolume name
      hv_list<- hypervolume_join(hv_list, hv_species) #add the hypervolume to the list
    }
  }
  return(hv_list)
}


