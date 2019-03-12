#' merge_data
#'
#' Combine multilibrary data into a master file.
#'
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param file_root Filename to write.
#'
#' @export

merge_data <- function(data, file_root){

  #prepare output data frame
  output_data <- data.frame(matrix(nrow = 0, ncol = ncol(data[[2]])))

  for(i in 2:length(data)){
    output_data %<>% rbind(data[[i]])
  }

  # output_name <- paste0(file_root,".tsv")
  output_name <- file_root
  data.table::fwrite(output_data, output_name, sep = "\t", na = "NA")
  return(output_data)

}
