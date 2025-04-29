#' Title
#'
#' @param abiotics_df abiotic variables data frame
#'
#' @return
#' @export
#'
#' @examples
input_pop_origin <- function(abiotics_df){


  #ask for latitude and longitude
  pop_lat <- as.numeric(dlgInput("Enter the sampling location latitude", "47.75")$res)
  pop_long <- as.numeric(dlgInput("Enter the sampling location longitude", "6.2500")$res)


  #get the closest cell to coordinates
  rank <- which(abs(abiotics_df$y - pop_lat) == min(abs(abiotics_df$y - pop_lat))&abs(abiotics_df$x - pop_long) == min(abs(abiotics_df$x - pop_long)))
  pop_lat <- abiotics_df$y[rank]
  pop_long <- abiotics_df$x[rank]


  #get the line of the abiotics_df corresponding to these coordinates
  pop_coords <- data.frame(y = pop_lat, x =pop_long)
  pop_param <- merge(pop_coords, abiotics_df, by=c("x","y"))
  write.table(pop_param, "population_origin_parameters.csv", row.names=FALSE, quote=T, sep =",", dec=".")

  return(pop_param)
}
