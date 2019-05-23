#' For multilibrary data, identify libraries, and call library_prosum
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param verbose Logical whether or not to print progress.
#' @param parallel Logical whether or not to use future and future.apply to parallelize individual library computations.
#'
#' @export

compute_prosum <- function(data, annot, verbose = TRUE, parallel = FALSE){
  # check for proper annotation order
  if(!is.null(annot)){
    if(mean(names(annot) == names(data)) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 names(annot), ";", names(data)))
    }
  }

  # prep output data list
  output_data <- list()
  libs <- names(data)

  # define function to be called by loop
  prepare_prosum <- function(data, annot, verbose, i){
    if(verbose) print(paste("Prosum:", libs[i],":",i,"of",length(libs)))
    library_prosum(data[[i]], annot[[i]], verbose)

  }

  # run loop
  if(parallel){


  } else{
    for(i in 1:length(libs)){
      output_data[[i]] <- prepare_prosum(data, annot, verbose, i)
    }
  }


  names(output_data) <- libs
  return(output_data)

}

#' For one peptide library, identify proteins from annotation file, and then
#' call protein_prosum for each protein.
#'
#' @param data Data frame with one library's data.
#' @param annot Annotation file.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

library_prosum <- function(data, annot, verbose = TRUE){

  pro_ids <- annot$pro_id[match(data[,1], annot$u_pep_id)]  %>% na.omit
  proteins <- unique(pro_ids)

  #setup prosum table
  lib_prosum <- data.frame(matrix(nrow = length(proteins), ncol = ncol(data)))
  colnames(lib_prosum) <- c("pro_id", colnames(data)[-1])
  lib_prosum[,1] <- proteins

  #loop through this librarys proteins
  if(verbose){pb <- pbar(0, length(proteins))}
  for(j in 1:length(proteins)){
    if(verbose){setTxtProgressBar(pb, j)}
    lib_prosum[j, -1] <- data[pro_ids == proteins[j], -1] %>%
      na.omit %>% protein_prosum
  }
  if(verbose) cat("\n")

  return(lib_prosum)

}

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
