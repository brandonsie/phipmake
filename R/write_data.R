#' A wrapper for data.table::fwrite to write file to drive.
#'
#'
#' @param data Data to write. If class of data is data.frame or data.table, then
#' write_data will fwrite. If class of data is a list, write_data will assume
#' that data is a phiplist and write the second through final elements as
#' separate files.
#' @param filename Filename or filenames to write.
#'
#' @export


write_data <- function(data, filename){

  if(class(data)[1] == "data.table" | class(data)[1] == "data.frame"){
    data.table::fwrite(data, filename, sep = "\t", na = "NA")
  } else if(class(data) == "list"){

    if(class(data[[1]]) == "character"){
      libs <- data[[1]]
      for(i in 1:length(libs)){
        if(!grepl(libs[i], filename[i])){
          stop(paste("Error: phipmake write_data: filename sublibrary mismatch:",
                     libs[i], filename[i]))
        }
        data.table::fwrite(data[[i+1]], filename[[i]], sep = "\t", na = "NA")
      }
    } else if(class(data[[1]]) == "data.table" | class(data[[1]]) == "data.frame"){
      libs <- names(data)
      for(i in 1:length(libs)){
        if(!grepl(libs[i], filename[i])){
          stop(paste("Error: phipmake write_data: filename sublibrary mismatch:",
                     libs[i], filename[i]))
        }
        data.table::fwrite(data[[i]], filename[[i]], sep = "\t", na = "NA")
      }

    }
  }
}