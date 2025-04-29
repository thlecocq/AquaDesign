#' Title Extract chosen parameters (read srad, vapr and prec) from WorldClim
#'
#' @return Returns a raster brick containing WorldClim data
#' @export
#'
#' @examples
WorldClim_select <- function(){
  #create a list of index to use in the loop
  months <-c("01","02","03","04","05","06","07","08","09","10","11","12")
  bio <-c("12", "13", "14", "15")

  #create empty raster brick with 12 layers, with nrow and ncol corresponding to 10 arcmins grid (before spatial filtering)
  srad<-brick(nrows=1080,ncol=2160,nl=length(months))
  vapr<-brick(nrows=1080,ncol=2160,nl=length(months))
  prec<-brick(nrows=1080,ncol=2160,nl=length(bio))

  #Unzip each layer/month, aggregate it and rename it
  for(i in 1:length(months)){
    name_srad <-paste0("wc2.1_10m_srad_",months[i],".tif")
    srad[[i]]<-raster(unzip("srad_zip.zip",name_srad))
    name_vapr <-paste0("wc2.1_10m_vapr_",months[i],".tif")
    vapr[[i]]<-raster(unzip("vapr_zip.zip",name_vapr))
  }

  #Unzip each layer/month and rename it
  for(i in 1:length(bio)){
    name_bio <-paste0("wc2.1_10m_bio_",bio[i],".tif")
    prec[[i]]<-raster(unzip("bio_zip.zip",name_bio))
  }

  #weird double assignation here
  #wc_files = c(srad, vapr, prec)
  #result <- wc_files

  result = c(srad, vapr, prec)
  return(result)

}
