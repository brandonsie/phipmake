#' getparam
#'
#' Function to retreive value of parameter from param name.
#'
#' @param params Data frame of parameters with two columns param and value.
#' @param param Character string corresponding to a value in param$param.
#'
#' @export



getparam <- function(params, param){params$value[params$param == param]}
