#' emphasize_hit_data_list
#'
#' Set non-hit values to a default value.
#'
#' @param data phiplist of raw data to use.
#' @param hits phiplist of binary data with 1 for hits and 0 for non-hits.
#' @param default Numeric value to replace non-hit values with.
#'
#' @export


emphasize_hit_data_list <- function(
  data, hits, default
){
  # check for proper data order
  if(mean(names(data) == names(hits)) < 1){
    stop(paste("Error: empahsize_hit_data_list: data and hits mismatch",
               names(data), ";", names(hits)))
  }

  # prep output data list
  output_data <- list()
  libs <- names(data)

  # run loop
  if(parallel){


  } else{
    for(i in 1:length(libs)){
      output_data[[i]] <- emphasize_hit_data(data[[i]], hits[[i]], default)
    }

  }

  names(output_data) <- libs
  return(output_data)




}

#' emphasize_hit_data
#'
#' Set non-hit values to a default value.
#'
#' @param data Data frame of raw data to use.
#' @param hits Data frame of binary data with 1 for hits and 0 for non-hits.
#' @param default Numeric value to replace non-hit values with.
#'
#' @export


emphasize_hit_data <- function(
  data, hits, default
){

  output_data <- data
  output_data[,-1][hits[,-1] == 0] <- default

  return(output_data)


}