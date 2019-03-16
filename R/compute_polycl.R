#' compute_polycl
#'
#' For multilibrary hits data, identify libraries, and call library_polycl
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param pairs Phiplist of blast alignment pair files.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

compute_polycl <- function(data, annot, pairs, verbose = TRUE){
  # check for proper annotation order
  if(!is.null(annot) & !is.null(pairs)){
    if(mean(annot[[1]] == data[[1]]) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 annot[[1]], ";", data[[1]]))
    } else if(mean(pairs[[1]] == data[[1]]) < 1){
      stop(paste("Error: annotate_data: pairs and data mismatch",
                 pairs[[1]], ";", data[[1]]))
    }
  }

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs <- data[[1]]

  for(i in 1:length(libs)){
    if(verbose) print(paste(libs[i],":",i,"of",length(libs)))

    sub.data <- data[[i+1]]
    sub.annot <- annot[[i+1]]
    sub.pairs <- annot[[i+1]]

    output_data[[i+1]] <- library_polycl(sub.data, sub.annot, sub.pairs, verbose)

  }

  return(output_data)

}
