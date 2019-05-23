#' swap out u_pep_id names for older pep_id names to improve AVARDA speed
#'
#' @param data Phiplist of data to modify.
#' @param annotations Phiplist of annotation files. Don't use with metadata_path param.
#' @param metadata_path Metadata directory. Don't use with annotations param.
#' @param replacement_column Column of metadata with which to replace current first column of data.
#'
#' @export


prepare_avarda_names <- function(data, annotations = NULL, metadata_path = NULL,
                                 replacement_column = "pep_id"){

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

  # loop over libraries as specified in data
  for(i in 1:length(libs)){

    # load annotation file
    if(!is.null(annotations)){
      annot <- annotations[[i]]
    } else if(!is.null(metadata_path)){
      annot <- read_annot(libs[i], metadata_path)
    }

    # process sublibrary data
    sub.data <- data[[i]]

    annot_orig <- annot[, names(annot) == names(sub.data)[1]]
    annot_replace <- annot[, names(annot) == replacement_column]

    sub.data[,1] <- annot_replace[match(sub.data[,1], annot_orig)]

    output_data[[i]] <- sub.data
  }

  names(output_data) <- names(data)
  return(output_data)

}