#' annotate_data
#'
#' Add peptide/protein annotation to data file.
#'
#' @param data Un-annotated data.
#' @param fields What information to populate from annotation file.
#' @param file_root Filename to write.
#' @param md_path Metadata directory.
#'
#' @export

annotate_data <- function(
  data, file_root,
  fields = c("pep_id", "pos_start", "pos_end",
             "UniProt_acc", "pep_aa", "taxon_species",
             "gene_symbol", "product"),
  md_path){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- data[[1]]


  # loop over libraries as specified in data[[1]]
  for(i in 2:length(data)){
    # load annotation file
    libname <- data[[1]][i-1] #get library basename from first list element of data
    annot <- read_annot(libname, md_path)

    # specify sublibrary data
    sublib_data <- data[[i]]

    # identify reference column (usually u_pep_id or pro_id)
    ref_col <- names(sublib_data)[1]

    # if using pro_id, remove peptide-specific fields
    # then get annotation file
    if(ref_col == "pro_id"){
      pep_fields <- c("pep_id", "pos_start", "pos_end", "pep_aa")
      fields <- fields[!(fields %in% pep_fields)]
    }
    annot_fields <- annot[match(sublib_data[,ref_col], annot[,ref_col]), fields]

    output_data[[i]] <- cbind(sublib_data[,1], annot_fields, sublib_data[,-1])
    names(output_data[[i]])[1] <- ref_col

    sub_name <- paste0(libname,"/",file_root,"_",libname,"_annotated.tsv")
    data.table::fwrite(output_data[[i]], sub_name, sep = "\t")


  }

  return(output_data)
}