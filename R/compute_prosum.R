#' For multilibrary data, identify libraries, and call library_prosum
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param verbose Logical whether or not to print progress.
#' @param parallel Logical whether or not to use foreach and doParallel to parallelize individual library computations.
#'
#' @export

compute_prosum <- function(data, annot, verbose = TRUE, parallel = FALSE){
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

  # define function to be called by loop
  prepare_prosum <- function(data, annot, verbose, i){
    if(verbose) print(paste("Prosum:", libs[i],":",i,"of",length(libs)))
    library_prosum(data[[i+1]], annot[[i+1]], verbose)

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
      output_data[[i+1]] <- prepare_prosum(data, annot, verbose, i)
    }

  } else{
    for(i in 1:length(libs)){
      output_data[[i+1]] <- prepare_prosum(data, annot, verbose, i)
    }
  }


  return(output_data)

}
