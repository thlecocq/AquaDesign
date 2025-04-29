#' Title Convert dataframe into PCA projection
#'
#' @param species_abiotics_df data frame with merged species and abiotics
#'
#'
#' @return
#' @export
#'
#' @examples
set_pca_axis <- function(species_abiotics_df){

  #run pca with scaling, do not take into account the first column of species_abiotics_df (species column)
  res <- prcomp(species_abiotics_df[-1], scale = TRUE)

  #Npc = number of principal components retained
  # Kaiser-Guttman rule is applied
  Npc <- as.numeric(summary(res$sd^2 > 1)["TRUE"])   #res$sd^2 = eigenvalue, count the number of dimensions for which the condition >1 is TRUE
  print(paste("using",Npc,"axes"))

  rescaled_abiotics <- res$x[,1:Npc] #in the pca, "x" is the matrix with the initial dataframe projected in the pca, keep only the "Npc" first columns

  rescaled_abiotics <- cbind(species_abiotics_df$species , as.data.frame(rescaled_abiotics)) #add species column to the new dataframe

  #set axes names
  axis_names <- c()
  for (i in 1:Npc){
    axis_names <- c(axis_names, paste0("Axis", i))
  }
  colnames(rescaled_abiotics) <- c("species", axis_names)

  return(rescaled_abiotics)
}
