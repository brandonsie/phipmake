#' Function to retreive value of parameter from param name.
#'
#' @param params Data frame of parameters with two columns param and value.
#' @param param Character string corresponding to a value in param$param.
#'
#' @export



getparam <- function(params, param){
  if((c("param", "value") %in% colnames(params)) %>% mean == 1){
    params$value[params$param == param]
  } else{
    params[,2][params[,1] == param]
  }

}
