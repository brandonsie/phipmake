#' trim alignment dictionary to not include self alignments or redundant values
#'
#' @param edge Input dictionary.
#'
#' @export


trimDictionary <- function(edge){
  #remove self alignments and redundant values from edge e.g. a==b and b==a
  edge <- edge[which(edge[,1]!=edge[,2]),] ## remove a == a
  edge <- edge[!duplicated(apply(edge,1,function(x){
    paste(sort(x),collapse='_')})),]

  return(edge)
}


