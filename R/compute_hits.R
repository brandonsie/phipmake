#' compute_hits
#'
#' For multilibrary enrichment, identify libraries, and identify values above threshold.
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param threshold Minimum enrichment value to be considered a hit.
#'
#' @export

compute_hits <- function(data, threshold = 10){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- data[[1]]

  for(i in 2:length(data)){
    lib_name <- data[[1]][i-1] #get library basename from first list element of data
    lib_data <- data[[i]]

    hits <- data.frame(matrix(nrow = nrow(lib_data), ncol = ncol(lib_data)))
    names(hits) <- names(lib_data)
    hits[,1] <- lib_data[,1]
    hits[,-1] <- (lib_data[,-1] > threshold) %>% as.numeric
    output_data[[i]] <- hits


    output_path <- paste0(lib_name,"/", "hits", "_", lib_name,"_promax.tsv")
    data.table::fwrite(output_data[[i]], output_path, sep = "\t")

  }


  return(output_data)

}