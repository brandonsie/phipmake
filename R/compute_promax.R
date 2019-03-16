#' compute_promax
#'
#' For multilibrary hits data, identify libraries, and call library_promax
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#'
#' @export

compute_promax <- function(data, annot){
  # calculate and write individual sublibrary promax files

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
    output_data[[i+1]] <- library_promax(data[[i+1]], annot[[i+1]])
}
  return(output_data)
}