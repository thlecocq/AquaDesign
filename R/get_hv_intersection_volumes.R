#' Title Generate a dataframe with all the combinations intersections volumes
#'
#' @param hv_list List; A list of hypervolumes
#' @param nb_combi Integer; The max number of species in combination (for example "4", will do all the combinations with 2 species, then all with 3, then all with 4)
#' @param priority_species List; A list of species name that must be in a combination for that combination to be retained (at least one species of this list)
#'
#' @return Returns a list of hypervolumes intersections for all combinations (rescaled between 0 and 1)
#' @export
#'
#' @examples

get_hv_intersection_volumes <- function(hv_list,nb_combi = NA,priority_species=NA){

  if (is.na(nb_combi)){
    #ask for the max number of species in combinations
    nb_combi <- dlg_list(title = "Chose the max number of species in combinations", c(2:(length(hv_list@HVList))))$res
    nb_combi <- as.numeric(nb_combi)
  }

  #get species_list from hv_list
  species_list <- c()
  for (i in 1:length(hv_list@HVList)){
    species_list <- c(species_list, hv_list[[i]]@Name)
  }

  num_list <- 1:length(species_list)

  #get all the combinations possible among the species of the list
  species_combi <- do.call("c", lapply(2:nb_combi, function(i) combn(species_list, i, FUN = list)))
  numbers_combi <- do.call("c", lapply(2:nb_combi, function(i) combn(num_list, i, FUN = list)))

  #if there is priorities species specified, remove the combinations which doesn't have any of them
  if (all(!is.na(priority_species))){
    indices_to_remove <- which(!sapply(species_combi, function(comb) any(comb %in% priority_species)))
	species_combi <- species_combi[-indices_to_remove]
	numbers_combi <- numbers_combi[-indices_to_remove]
  }

  #inform about the number of combinations
  number_of_combinations_to_calculate=length(species_combi)
  print(paste0("combinations to calculate :" , as.character(number_of_combinations_to_calculate)))

  #create an empty result dataframe
  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))
  for ( i in 1:length(species_combi)){
    vect <- species_combi[[i]]
    length(vect) <- nb_combi+1
    combi_df <- rbind(combi_df, vect)
  }

  #replace NAs by "none" to be able to make comparisons later
  combi_df[is.na(combi_df)] <- "None"

  #for each combination
  for (i in 1:length(numbers_combi)){
	indices=unlist(numbers_combi[i])
    #make a list with the hypervolumes to compare and run the comparison function, add the volume to the dataframe
    hv_list_test <-  hv_list[[indices]]
    intersection <- hypervolume_set_n_intersection(hv_list_test)
    combi_df[[nb_combi+1]][i] <- intersection@Volume
    #progress
    if(i%%100==0){
	progression=paste0("Hypervolumes intersections calculations... ",i,"/",number_of_combinations_to_calculate)
	print(progression)
  }
    
  }
  #print the last progression, if needed
  if(number_of_combinations_to_calculate%%100!=0){
	progression=paste0("Hypervolumes intersections calculations... ",number_of_combinations_to_calculate,"/",number_of_combinations_to_calculate)
	print(progression)
  }
  
  #rescale the volumes between 0 and 1
  combi_df[nb_combi+1] <- as.numeric(combi_df[[nb_combi+1]])
  rescaled_combi_df <-cbind(combi_df[1:nb_combi], apply(combi_df[nb_combi+1], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

  return(rescaled_combi_df)
}
