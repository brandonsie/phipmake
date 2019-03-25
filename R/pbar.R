#' Wrapper for txtProgressBar with some defualt settings.
#'
#' @param min Progress bar minimum value.
#' @param max Progress bar maximum value.
#'
#' @export
#'
#'


pbar <- function(min,max){
  txtProgressBar(min = min, max = max, initial = min,
                 char = "=", width = NA, style = 3, file = "")
}
