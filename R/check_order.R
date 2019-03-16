#' check_order
#'
#' Maintains specified order of panlibrary data after a call to dplyr::bind_rows.
#'
#' @param libs Character vector of library ID's in order
#' @param data Merged data to re-sort.
#'
#' @export

check_order <- function(libs, data){
  sorted_data <- data.frame(matrix(nrow = 0, ncol = ncol(data)))

  for(i in libs){
    sorted_data %<>% rbind(data[grep(i, data[,1]),])
  }

}

