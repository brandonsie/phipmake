#' read_pairs_list
#'
#' Read blast alignment pair files based on library names and metadata path.
#'
#' @param libs Character vector of library names.
#' @param pairs_path Metadata directory as character string.
#'
#' @return phiplist of blast pairs First element is character vector of
#' libraries from libs input. Remaining elements are corresponding alignment
#' files.
#'
#' @export



read_pairs_list <- function(libs, pairs_path){
  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs

  for(i in 1:length(libs)){
    output_data[[i+1]] <- read_pairs(libs[i], pairs_path)
  }

  return(output_data)

}