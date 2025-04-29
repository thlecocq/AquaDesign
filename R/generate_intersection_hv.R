#' Title Calculate the intersection of hypervolumes
#'
#' @param hv_list list of species hypervolumes
#'
#' @return Returns a list of hypervolumes intersections
#' @export
#'
#' @examples
generate_intersection_hv <- function (hv_list){

  #ask for the max number of species in combinations
  nb_combi <- dlg_list(title = "Chose the max number of species in combinations",
                       c(2:(length(hv_list@HVList))))$res
  nb_combi <- as.numeric(nb_combi)

  species_list_hv_selected <- c()
  for (i in 1:length(hv_list@HVList)) {
    #store the species names in a list
    species_list_hv_selected <- c(species_list_hv_selected,
                                  hv_list[[i]]@Name)
  }

  #get all the possible combinations from the list from single species to combinations of "nb_combi" species
  list_combi <- do.call("c", lapply(1:nb_combi, function(i) combn(species_list_hv_selected,
                                                                  i, FUN = list)))
  #remove the single species from the list (which are the first elements)
  list_combi <- list_combi[-(1:length(species_list_hv_selected))]

  #inform about the number of combinations
  print(paste0("combinations to calculate :", as.character(length(list_combi))))

  #Initialize a list and a dataframe
  hv_intersection_list <- new("HypervolumeList")
  combi_df <- data.frame(matrix(ncol = nb_combi, nrow = 0))

  #convert the combinations list into a dataframe
  for (i in 1:length(list_combi)) {
    vect <- list_combi[[i]]
    length(vect) <- nb_combi + 1
    combi_df <- rbind(combi_df, vect)
  }
  #replace NAs by "none" to be able to make comparisons later
  combi_df[is.na(combi_df)] <- "None"

  #for each combination
  for (i in 1:length(combi_df[, 1])) {

    #Initialize and fill a list of indexes corresponding of the Hypervolumes to compare  in combination number i
    ind <- c()
    hv_list_test <- new("HypervolumeList")
    for (j in 1:length(species_list_hv_selected)) {
      for (q in 1:length(combi_df[1, ])) {
        if (combi_df[[q]][i] == hv_list[[j]]@Name) {
          ind <- c(ind, j)
        }
      }
    }

    #make a list with the hypervolumes to compare and run the comparison function
    hv_list_test <- hv_list[[ind]]
    intersection <- hypervolume_set_n_intersection(hv_list_test)

    #set intersection hypervolumes names
    hv_name <- c()
    for (k in 1:length(list_combi[[i]])){
      hv_name <- paste(hv_name,list_combi[[i]][k])
    }
    intersection@Name <- hv_name

    #add the intersection hypervolume in the hypervolume list
    hv_intersection_list <- hypervolume_join(hv_intersection_list, intersection)
  }

  return(hv_intersection_list)
}
