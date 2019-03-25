#' Take data frame input and calculate the polyclonal score in each column or row.
#'
#' @param data Data frame of hits from which to calculate polyclonal score. The first column should contain peptide names. Each subsequent column should represent data for one sample and each row should represent data for one peptide. All peptides in data should be from the same protein.
#' @param pairs 2 column table of aligning pairs that should not be double-counted for polyclonal score. For independence_filter method, this data should contain no redundancy e.g. a==b and b==a or self alignments e.g. a==a
#' @param margin Numeric 1 for row 2 for column.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#'
#' @return polycl_scores; A numeric vector with length equal to the number of samples represented in the data parameter. One polyclonal score for each patient, collapsed from all input peptide hits data.
#'
#' @export
#'


protein_polycl <- function(data, pairs, margin = 2, method = "independence_filter"){
  options(stringsAsFactors = FALSE)
  # require(magrittr)

  peptides <- data[,1] #peptide names
  sub_pairs <- pairs[pairs[,1] %in% peptides,] #subset relevant blast alignment pairs
  polycl_scores <- rep(0, (ncol(data)-1))

  # get hits for that sample
  # check which hits align to each other & collapse

  # check for trivial CASES
  if(nrow(data) == 0){
    warning("Warning: no peptide data input to protein_polycl. (i.e., parameter data has 0 rows).")

  } else if((nrow(sub_pairs) == 0) | (nrow(data) == 1)){
    # if none of this protein's peptides align to each other,
    # or if this protein only has one peptide then
    # polyclonal score <- hits score
    polycl_scores <- data[,-1]

  } else{
    for(i in 2:ncol(data)){
      # loop to process each sample separately

      # identify which peptides were hits for current sample
      sample_data <- as.numeric(data[,i])
      hits <- peptides[sample_data == 1]

      if(length(hits) == 0){
        polycl_scores[i-1] <- 0

      } else if(method == "independence_filter"){

        polycl_scores[i-1] <- independence_filter(sub_pairs, hits)

      } else if(method == "old_method"){
        # initialize a vector to keep track of which hits have either been
        # counted for polyclonal or blast align to another hit that has been
        # counted for polyclonal.

        explained_hits <- vector(mode = "character")
        polycl_scores[i-1] <- 0

        for(j in hits){
          if(!(j %in% explained_hits)){
            polycl_scores[i-1] %<>% + 1
            aligning_peptides <- sub_pairs[sub_pairs[,1] == j, 2]
            explained_hits %<>% c(aligning_peptides)
          }
        }

      } else stop("Error: invalid method supplied to protein_polycl.")

    }
  }

  return(polycl_scores) #note this returns values only, no protein name
}