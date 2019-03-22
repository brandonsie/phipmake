#' compute_polycl
#'
#' For multilibrary hits data, identify libraries, and call library_polycl
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param pairs Phiplist of blast alignment pair files.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

compute_polycl <- function(data, annot, pairs, method = "independence_filter", verbose = TRUE){
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
    if(verbose) print(paste("Polyclonal:", libs[i],":",i,"of",length(libs)))
    output_data[[i+1]] <- library_polycl(data[[i+1]],  annot[[i+1]], pairs[[i+1]], method, verbose)

  }

  return(output_data)

}
