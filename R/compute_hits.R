#'  For multilibrary enrichment, identify libraries, and identify values above threshold.
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param threshold Minimum enrichment value to be considered a hit.
#' @param parallel Logical whether or not to use future and future.apply to parallelize individual library computations.
#' @param verbose Logical whether to print additional info
#'
#' @export

compute_hits <- function(data, threshold = 5, parallel = FALSE, verbose = FALSE){

  # prep output data list
  output_data <- list()
  libs <- names(data)



  if(parallel){

  } else{
    for(i in 1:length(libs)){
      if(verbose) print(paste("Hits:", libs[i],":",i,"of",length(libs)))
      sub.data <- data[[i]]
      hits <- data.frame(matrix(nrow = nrow(sub.data), ncol = ncol(sub.data)))
      names(hits) <- names(sub.data)
      hits[,1] <- sub.data[,1]
      hits[,-1] <- (sub.data[,-1] > threshold) %>% as.numeric
      output_data[[i]] <- hits
    }
  }

  names(output_data) <- names(data)
  return(output_data)
}