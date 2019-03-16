#' library_polycl
#'
#' For one peptide library, identify proteins from annotation file, and then
#' call protein_polycl for each protein.
#'
#' @param data Data frame with one library's hits data
#' @param annot Annotation file.
#' @param pairs Data frame of BLAST aligning pairs within each protein.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

library_polycl <- function(data, annot, pairs, verbose = TRUE){
  if(verbose){
    print("data dim:")
    print(dim(data))

    print("annot dim:")
    print(dim(annot))

    print("pairs dim:")
    print(dim(pairs))
  }

  pro_ids <- annot$pro_id[match(data[,1], annot$u_pep_id)]  %>% na.omit
  proteins <- annot$pro_id %>% unique

  #setup polyclonal table
  lib_polycl <- data.frame(matrix(nrow = length(proteins), ncol = ncol(data)))
  colnames(lib_polycl) <- c("pro_id", colnames(data)[-1])
  lib_polycl[,1] <- proteins

  if(verbose){
    print(paste("length pro ids", length(pro_ids)))
    print(paste("length proteins:", length(proteins)))
    print("dim lib polycl:")
    print(dim(lib_polycl))
  }

  #loop through this library's proteins
  if(verbose){pb <- pbar(0, length(proteins))}
  for(i in 1:length(proteins)){
    if(verbose & interactive()){
      setTxtProgressBar(pb, i)
    } else if(verbose & (i %% 100 == 0)){
      setTxtProgressBar(pb, i)
    }


    lib_polycl[i, -1] <- data[c(1:length(pro_ids))[
      pro_ids == proteins[i]], ] %>% na.omit %>% protein_polycl(pairs) #in bracket , used to be ...  == proteins[i]], -1]. remove d-1 to feed to protein polycl
  }
  if(verbose) cat("\n")

  return(lib_polycl)
}



