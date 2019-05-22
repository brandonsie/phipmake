#' For multilibrary hits data, identify libraries, and call library_promax
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param verbose Logical whether or not to print progress.
#' @param parallel Logical whether or not to use future and future.apply to parallelize individual library computations.
#'
#' @export

compute_promax <- function(data, annot, verbose = TRUE, parallel = FALSE){
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
  prepare_promax <- function(data, annot, verbose, i){
    if(verbose) print(paste("Promax:", libs[i],":",i,"of",length(libs)))
    library_promax(data[[i]], annot[[i]], verbose)

  }

  # run loop
  if(parallel){


  } else{
    for(i in 1:length(libs)){
      output_data[[i]] <- prepare_promax(data, annot, verbose, i)
    }

  }

  names(output_data) <- libs
  return(output_data)
}

#' For one peptide library, identify proteins from annotation file, and then
#' call protein_promax for each protein.
#'
#' @param data Data frame with one library's data.
#' @param annot Annotation file.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

library_promax <- function(data, annot, verbose = TRUE){

  pro_ids <- annot$pro_id[match(data[,1], annot$u_pep_id)]  %>% na.omit
  proteins <- unique(pro_ids)

  #setup promax table
  lib_promax <- data.frame(matrix(nrow = length(proteins), ncol = ncol(data)))
  colnames(lib_promax) <- c("pro_id", colnames(data)[-1])
  lib_promax[,1] <- proteins

  #loop through this librarys proteins
  if(verbose){pb <- pbar(0, length(proteins))}
  for(j in 1:length(proteins)){
    if(verbose){setTxtProgressBar(pb, j)}
    lib_promax[j, -1] <- data[pro_ids == proteins[j], -1] %>%
      na.omit %>% protein_promax
  }
  if(verbose) cat("\n")
  return(lib_promax)

}

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