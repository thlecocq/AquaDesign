#' Title Project user's system into PCA space
#'
#' @param species_abiotics_df
#' @param user_param_syst
#'
#' @return
#' @export
#'
#' @examples
set_system <- function(species_abiotics_df, user_param_syst){

  # Guttman-Kaiser rule
  res <- prcomp(species_abiotics_df[-1], scale = TRUE)#remove first column (species)
  Npc <- as.numeric(summary(res$sd^2 > 1)["TRUE"])   #Npc = number of principal components retained, res$sd^2 = eigenvalue

  print(paste("using",Npc,"axes"))

  # project new data onto the PCA space
  user_param_rescaled <- predict(res, user_param_syst)

  user_param_rescaled <- user_param_rescaled[,1:Npc]#only keep the "Npc" first columns/dimensions

  return(user_param_rescaled)

}

