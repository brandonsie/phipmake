#' For multilibrary hits data, identify libraries, and call library_promax
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

compute_promax <- function(data, annot, verbose = TRUE){
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
    if(verbose) print(paste("Promax:", libs[i],":",i,"of",length(libs)))
    output_data[[i+1]] <- library_promax(data[[i+1]], annot[[i+1]], verbose)
}
  return(output_data)
}