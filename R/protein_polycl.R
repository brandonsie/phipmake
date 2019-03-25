#' protein_polycl
#'
#' Take data frame input and calculate the polyclonal score in each column or row.
#'
#' @param data Data frame of numeric values for which to calculate maximum.
#' @param pairs 2 column table of aligning pairs that should not be double-counted for polyclonal score. For independence_filter method, this data should contain no redundancy e.g. a==b and b==a or self alignments e.g. a==a
#' @param margin Numeric 1 for row 2 for column.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#'
#' @export
#'


protein_polycl <- function(data, pairs, margin = 2, method = "independence_filter"){
  # #take single protein, multi patient data. cunalc max of EVERY column (separately)
  # apply(data, margin, function(x){x %>% na.omit %>% max})
  options(stringsAsFactors = FALSE)
  library(magrittr)

  peptides <- data[,1]
  sub_pairs <- pairs[pairs[,1] %in% peptides,]

  # get hits for that person
  # check which hits align

  if(nrow(sub_pairs) == 0){
    polycl_scores <- data[,-1]
  } else{
    for(x in 1:ncol(data)){

    }

    polycl_scores <- apply(data[,-1], margin, function(x){
      # for this protein, for this sample, list peptide hits

      if(length(x) == 0){
        independent_hit_count <- 0
      } else if(length(x) == 1){
        independent_hit_count <- x
      } else{
        hits <- data[,1][as.numeric(x) == 1]


        if(method == "independence_filter"){
          if(length(hits) > 0){
            independent_hit_count <- independence_filter(pairs, hits)
          } else independent_hit_count <- 0

        } else if(method == "old_method"){
          # initialize a vector to keep track of which hits have either been counted for polyclonal or blast align to another hit that has been counted for polyclonal.
          explained_hits <- vector(mode = "character")
          independent_hit_count <- 0

          for(i in hits){
            if(!(i %in% explained_hits)){
              independent_hit_count %<>% + 1
              aligning_peptides <- sub_pairs[sub_pairs[,1] == i, 2]
              explained_hits %<>% c(aligning_peptides)
            }
          }
        }
      }



      return(independent_hit_count)
    })

  }

  return(polycl_scores) #note this returns values only, no protein name
}