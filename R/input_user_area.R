#' Title Filter the input by min and max latitude and longitude (asked by prompt)
#'
#' @param abiotics_df abiotic variables data frame
#'
#' @return Returns the abiotic dataframe filtered by the latitutde and longitude specified
#' @export
#'
#' @examples
input_user_area <- function(abiotics_df){
  min_lat <- as.numeric(dlgInput("Enter the area's minimum latitude", "47.75")$res)
  min_long <- as.numeric(dlgInput("Enter the area's minimum longitude", "6.2500")$res)
  max_lat <- as.numeric(dlgInput("Enter the area's maximum latitude", "57.75")$res)
  max_long <- as.numeric(dlgInput("Enter the area's maximum longitude", "10.2500")$res)

  #get the line of the abiotics_df corresponding to these coordinates

  area_param <- abiotics_df %>%
    dplyr::filter(y<max_lat, y>(min_lat), x>(min_long), x<max_long)


  return(area_param)

}
