#' For one peptide library, identify proteins from annotation file, and then
#' call protein_polycl for each protein.
#'
#' @param data Data frame with one library's hits data
#' @param annot Annotation file.
#' @param pairs Data frame of BLAST aligning pairs within each protein.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

library_polycl <- function(data, annot, pairs, method = "independence_filter", verbose = TRUE){

  pro_ids <- annot$pro_id[match(data[,1], annot$u_pep_id)]  %>% na.omit
  proteins <- annot$pro_id %>% unique

  #setup polyclonal table
  lib_polycl <- data.frame(matrix(nrow = length(proteins), ncol = ncol(data)))
  colnames(lib_polycl) <- c("pro_id", colnames(data)[-1])
  lib_polycl[,1] <- proteins

  #loop through this librarys proteins
  if(verbose){pb <- pbar(0, length(proteins))}
  for(i in 1:length(proteins)){
    print(i)
    if(verbose){setTxtProgressBar(pb, i)}
    lib_polycl[i, -1] <- data[c(1:length(pro_ids))[
      pro_ids == proteins[i]], ] %>% na.omit %>% protein_polycl(pairs, method)
  }
  if(verbose) cat("\n")

  return(lib_polycl)
}



