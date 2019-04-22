#' For multilibrary hits data, identify libraries, and call library_polycl
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param pairs Phiplist of blast alignment pair files.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#' @param verbose Logical whether or not to print progress.
#' @param parallel Logical whether or not to use foreach and doParallel to parallelize individual library computations.
#'
#' @export

compute_polycl <- function(data, annot, pairs, method = "independence_filter", verbose = TRUE, parallel = FALSE){
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

  # define function to be called by loop
  prepare_polycl <- function(data, annot, pairs, method, verbose, i){
    if(verbose) print(paste("Polyclonal:", libs[i],":",i,"of",length(libs)))
    library_polycl(data[[i+1]],  annot[[i+1]], pairs[[i+1]], method, verbose)

  }

  # run loop
  if(parallel){
    require(foreach)
    require(doParallel)

    ncores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl, cores = ncores)
    print(paste("Paralellization registered with cores = ", ncores))

    foreach::foreach(i = 1:length(libs)) %dopar% {
      output_data[[i+1]] <- prepare_polycl(data, annot, pairs, method, verbose, i)
    }

  } else{
    for(i in 1:length(libs)){
      output_data[[i+1]] <- prepare_polycl(data, annot, pairs, method, verbose, i)
    }

  }

  return(output_data)

}
