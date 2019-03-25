#'  For multilibrary enrichment, identify libraries, and identify values above threshold.
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param threshold Minimum enrichment value to be considered a hit.
#'
#' @export

compute_hits <- function(data, threshold = 5){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs <- data[[1]]

  for(i in 1:length(libs)){
    sub.data <- data[[i + 1]]
    hits <- data.frame(matrix(nrow = nrow(sub.data), ncol = ncol(sub.data)))
    names(hits) <- names(sub.data)
    hits[,1] <- sub.data[,1]
    hits[,-1] <- (sub.data[,-1] > threshold) %>% as.numeric
    output_data[[i+1]] <- hits
  }
  return(output_data)
}