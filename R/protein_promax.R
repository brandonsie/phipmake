#' protein_promax
#'
#' Take data frame input and calculate the maximum value in each column or row.
#'
#' @param data Data frame of numeric values for which to calculate maximum.
#' @param margin Numeric 1 for row 2 for column.
#'
#' @export
#'


protein_promax <- function(data, margin = 2){
  #take single protein, multi patient data. calc max of EVERY column (separately)
  apply(data, margin, function(x){x %>% na.omit %>% max})
}