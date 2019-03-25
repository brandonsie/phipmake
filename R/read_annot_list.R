#' Read annotation files based on library names and metadata path.
#'
#' @param libs Character vector of library names.
#' @param md_path Metadata directory as character string.
#'
#' @return phiplist of annotations. First element is character vector of
#' libraries from libs input. Remaining elements are corresponding annotation
#' files.
#'
#' @export



read_annot_list <- function(libs, metadata_path){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs

  for(i in 1:length(libs)){
    output_data[[i+1]] <- read_annot(libs[i], metadata_path)
  }

  return(output_data)

}