#' Title Download datasets from various databases if the files do not already exist on hard drive
#'
#' @param url String; Url to download
#' @param filePath String; Output path of the downloaded file
#'
#'@importFrom utils download.file
#' @return
#' @export
#'
#' @examples

checkAndDownload <- function(url,filePath) {
    options(timeout = 1000)
    if (!file.exists(filePath)) {
        tryCatch(
            {
                download.file(url,filePath, mode = "wb", quiet = FALSE)
            }, 
            error = function(e){
                print(paste0("The download failed on url ",url))
                quit()
            }
        )
    }
    else{
      warningMsg=paste0("The file ",filePath," already exist. If you wish to update it, please remove this file and rerun Aquadesign.\n")
      cat(warningMsg)
    }
}
