#' Title Extract chosen paramters (annual_meanT, T_seasonality, maxT_WM, minT_CM, annual_rangeT, meanT_DQ, ph_max, elevation_avg, slope_avg) from EarthEnv
#'
#' @return A raster brick containing EarthEnv data
#' @import raster
#' @import ncdf4
#' @export
#'
#' @examples
#'
EarthEnv_select <- function(){
  earthenv_files_list <- list("hydro_avg", "soil_max", "elevation", "slope")
  for (i in 1:length(earthenv_files_list)){
    assign(as.character(earthenv_files_list[i]), brick(paste0(earthenv_files_list[i],".nc")) )
  }
  #selecting and renaming the factors
  hydro_selected <-hydro_avg[[c(1,4,5,6,7,9)]]
  soil_max_selected <- soil_max[[c(2)]]
  elevation_selected <- elevation[[c(4)]]
  slope_selected <- slope[[c(4)]]
  names(hydro_selected) <-paste(c("annual_meanT", "T_seasonality", "maxT_WM", "minT_CM", "annual_rangeT", "meanT_DQ"))
  names(soil_max_selected) <- paste(c("ph_max"))
  names(elevation_selected) <- paste(c("elevation_avg"))
  names(slope_selected) <- paste(c("slope_avg"))

  #stack the selected layers in a raster brick
  earthenv_data <- stack(hydro_selected, soil_max_selected, elevation_selected, slope_selected)
  return(earthenv_data)
}
