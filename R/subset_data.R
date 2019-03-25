#' Pull out specific sublibrary data from a larger data frame based on first column grep.
#'
#'
#' @param data Multilibrary data frame.
#' @param sub Subset term to grep for in first column of data.
#' @param filename Filename to write.
#'
#' @export


subset_data <- function(data, sub, filename){

  sub.data <- data[grep(sub, data[,1]),]
  names(sub.data)[1] <- "u_pep_id" #coerce first column name to be proper

  data.table::fwrite(sub.data, filename, sep = "\t")

  output_data <- list(sub, sub.data)

  return(output_data)

}