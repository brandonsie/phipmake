#' compute_promax
#'
#' For multilibrary hits data, identify libraries, and call library_promax
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param file_root Filename to write.
#' @param md_path Metadata directory.
#'
#' @export

compute_promax <- function(data, file_root, md_path){
  # calculate and write individual sublibrary promax files

  # prep output data list
  output_data <- list()
  output_data[[1]] <- data[[1]]

  for(i in 2:length(data)){

    lib_name <- data[[1]][i-1] #get library basename from first list element of data
    print(lib_name)
    annot <- read_annot(lib_name, md_path)

    output_data[[i]] <- library_promax(data[[i]], annot)

    promax_path <- paste0(lib_name,"/", file_root, "_", lib_name,"_promax.tsv")
    data.table::fwrite(output_data[[i]], promax_path, sep = "\t")
  }

  return(output_data)
}