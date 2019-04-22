#'  For multilibrary enrichment, identify libraries, and identify values above threshold.
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param threshold Minimum enrichment value to be considered a hit.
#' @param parallel Logical whether or not to use foreach and doParallel to parallelize individual library computations.
#'
#' @export

compute_hits <- function(data, threshold = 5, parallel = FALSE){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs <- data[[1]]



  if(parallel){
    require(foreach)
    require(doParallel)

    ncores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl, cores = ncores)
    print(paste("Paralellization registered with cores = ", ncores))

    foreach::foreach(i = 1:length(libs)) %dopar% {
      if(verbose) print(paste("Hits:", libs[i],":",i,"of",length(libs)))
      sub.data <- data[[i + 1]]
      hits <- data.frame(matrix(nrow = nrow(sub.data), ncol = ncol(sub.data)))
      names(hits) <- names(sub.data)
      hits[,1] <- sub.data[,1]
      hits[,-1] <- (sub.data[,-1] > threshold) %>% as.numeric
      output_data[[i+1]] <- hits
    }

  } else{
    for(i in 1:length(libs)){
      if(verbose) print(paste("Hits:", libs[i],":",i,"of",length(libs)))
      sub.data <- data[[i + 1]]
      hits <- data.frame(matrix(nrow = nrow(sub.data), ncol = ncol(sub.data)))
      names(hits) <- names(sub.data)
      hits[,1] <- sub.data[,1]
      hits[,-1] <- (sub.data[,-1] > threshold) %>% as.numeric
      output_data[[i+1]] <- hits
    }
  }

  return(output_data)
}