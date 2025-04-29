#' Title Extract chosen paramters (all) from FLO1K
#'
#' @return A raster brick containing FLO1K data
#' @export
#' @importFrom utils unzip
#' @param resolution Integer; Chosen resolution (10 or 30, default 30)
#'
#' @examples
FLO1K_select <- function(resolution=30) {

  flo1k_files_names <- c("av", "mi", "ma")

  flo1k_files = c()
  
  #for each avg/min/max, open the file as a raster, convert it into data frame and add it to abotics_df
  for(i in 1:length(flo1k_files_names)){
    if (resolution == 30){
      name_flow <-paste0("FLO1K.30min.ts.1960.2015.q",flo1k_files_names[i],".nc")
      flow <-raster(unzip("flow_30_zip.zip",name_flow))
    }
    if (resolution == 10){
      name_flow <-paste0("FLO1K.5min.ts.1960.2015.q",flo1k_files_names[i],".nc")
      flow <-raster(unzip("flow_5_zip.zip",name_flow))
    }
    flo1k_files = c(flo1k_files, flow)
  }

  result <- flo1k_files
  return(result)
}
