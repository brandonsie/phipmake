#' Add peptide/protein annotation to data file.
#'
#'@param libname Library name contained in data.
#' @param data Phiplist of unannotated edata.
#' @param annotations Phiplist of annotation files. Don't use with metadata_path param.
#' @param metadata_path Metadata directory. Don't use with annotations param.
#' @param fields What information to populate from annotation file.
#' @param parallel Logical whether or not to use future and future.apply to parallelize individual library computations.
#'
#' @return
#'
#' @export

annotate_data <- function(
  data, annotations = NULL, metadata_path = NULL,
  fields = c("pep_id", "pos_start", "pos_end",
             "UniProt_acc", "pep_aa", "taxon_genus", "taxon_species",
             "gene_symbol", "product"),
  pep_fields = c("pep_id", "pos_start", "pos_end", "pep_aa"),
  parallel = FALSE
  ){

  # check for proper annotation order
  if(!is.null(annotations)){
    if(mean(names(annotations) == names(data)) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 names(annotations), ";", names(data)))
    }
  }

  # prep output data list
  output_data <- list()
  libs <- names(data)

  # define function to be called by loop
  prepare_annotations <- function(
    data, annotations, metadata_path, fields, i){

    # load annotation file
    if(!is.null(annotations)){
      annot <- annotations[[i]]
    } else if(!is.null(metadata_path)){
      annot <- read_annot(libs[i], metadata_path)
    }

    # process sublibrary data
    sub.data <- data[[i]]
    ref_col <- names(sub.data)[1]

    # if using pro_id, remove peptide-specific fields. Then annotate.
    if(ref_col == "pro_id"){fields <- fields[!(fields %in% pep_fields)]}
    annot_fields <- annot[match(sub.data[,ref_col], annot[,ref_col]), fields]
    annot_fields[,] <- lapply(annot_fields[,], as.character)


    # combine annotation and original data, prepare output
    sub.output <- cbind(sub.data[,1], annot_fields, sub.data[,-1])
    names(sub.output)[1] <- names(sub.data)[1]
    return(sub.output)
  }

  # loop over libraries as specified in data[[1]]
  if(parallel){


  } else{
    for(i in 1:length(libs)){
      output_data[[i]] <- prepare_annotations(
        data, annotations, metadata_path, fields, i)
    }
  }

  names(output_data) <- names(data)
  return(output_data)
}