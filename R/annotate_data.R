#' annotate_data
#'
#' Add peptide/protein annotation to data file.
#'
#'@param libname Library name contained in data.
#' @param data Phiplist of unannotated edata.
#' @param annotations Phiplist of annotation files. Don't use with metadata_path param.
#' @param metadata_path Metadata directory. Don't use with annotations param.
#' @param fields What information to populate from annotation file.
#'
#' @export

annotate_data <- function(
  data, annotations = NULL, metadata_path = NULL,
  fields = c("pep_id", "pos_start", "pos_end",
             "UniProt_acc", "pep_aa", "taxon_species",
             "gene_symbol", "product")
  ){

  # check for proper annotation order
  if(!is.null(annotations)){
    if(mean(annotations[[1]] == data[[1]]) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 annotations[[1]], ";", data[[1]]))
    }
  }

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs <- data[[1]]

  # loop over libraries as specified in data[[1]]
  for(i in 1:length(libs)){

    # load annotation file
    libname <- libs[i] #get library basename from first list element of data


    if(!is.null(annotations)){
      annot <- annotations[[i+1]]
    } else if(!is.null(metadata_path)){
      annot <- read_annot(libname, metadata_path)
    }


    # process sublibrary data
    sub.data <- data[[i+1]]
    ref_col <- names(sub.data)[1]

    # if using pro_id, remove peptide-specific fields. Then annotate.
    if(ref_col == "pro_id"){
      pep_fields <- c("pep_id", "pos_start", "pos_end", "pep_aa")
      fields <- fields[!(fields %in% pep_fields)]
    }
    annot_fields <- annot[match(sub.data[,ref_col], annot[,ref_col]), fields]

    # combine annotation and original data, prepare output
    sub.output <- cbind(sub.data[,1], annot_fields, sub.data[,-1])
    names(sub.output)[1] <- names(sub.data)[1]
    output_data[[i+1]] <- sub.output

    # data.table::fwrite(output_data, filename, sep = "\t", na = "NA")
  }
  return(output_data)
}