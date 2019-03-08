#' protein_prosum
#'
#' Take data frame input and calculate the sum of values in each column or row.
#'
#' @param data Data frame of numeric values for which to calculate sum
#' @param margin Numeric 1 for row 2 for column.
#'
#' @export
#'
#'

protein_prosum <- function(data, margin = 2){
  #take single protein, multi patient data. calc max of EVERY column (separately)
  apply(data, 2, function(x){x %>% na.omit %>% sum})
}
