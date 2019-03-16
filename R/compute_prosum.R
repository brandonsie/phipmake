#' compute_prosum
#'
#' For multilibrary data, identify libraries, and call library_prosum
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#'
#' @export

compute_prosum <- function(data, annot){
  # check for proper annotation order
  if(!is.null(annot)){
    if(mean(annot[[1]] == data[[1]]) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 annot[[1]], ";", data[[1]]))
    }
  }

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs <- data[[1]]

  for(i in 1:length(libs)){
    sub.data <- data[[i+1]]
    sub.annot <- annot[[i+1]]

    output_data[[i+1]] <- library_prosum(sub.data, sub.annot)
  }

  return(output_data)

}
