#' compute_prosum
#'
#' For multilibrary data, identify libraries, and call library_prosum
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param file_root Filename to write.
#' @param md_path Metadata directory.
#'
#' @export

compute_prosum <- function(data, file_root, md_path){
  # prep output data list
  output_data <- list()
  output_data[[1]] <- data[[1]]

  for(i in 2:length(data)){

    lib_name <- data[[1]][i-1] #get library basename from first list element of data
    print(lib_name)
    annot <- read_annot(lib_name, md_path)

    output_data[[i]] <- library_prosum(data[[i]], annot)

    output_path <- paste0(lib_name,"/", file_root, "_", lib_name,"_prosum.tsv")
    data.table::fwrite(output_data[[i]], output_path, sep = "\t")
  }

  return(output_data)

}
