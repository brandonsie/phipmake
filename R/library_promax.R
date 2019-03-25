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
