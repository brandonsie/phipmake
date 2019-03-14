#' compute_polycl
#'
#' For multilibrary hits data, identify libraries, and call library_polycl
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param file_root Filename to write.
#' @param md_path Metadata directory.
#' @param pairs_path BLAST pairs directory.
#'
#' @export

compute_polycl <- function(data, file_root, md_path, pairs_path){
  # prep output data list
  print("Running compute_polycl")

  output_data <- list()
  output_data[[1]] <- data[[1]]

  for(i in 2:length(data)){
    lib_name <- data[[1]][i-1] #get library basename from first list element of data
    print(paste("i:", i))
    print(lib_name)

    print("reading annotation file")
    annot <- read_annot(lib_name, md_path)
    print("reading pairs file")
    pairs <- read_pairs(lib_name, pairs_path)
    print("done reading pairs file")

    output_data[[i]] <- library_polycl(data[[i]], annot, pars)

    output_path <- paste0(lib_name,"/", file_root, "_", lib_name,"_polycl.tsv")
    data.table::fwrite(output_data[[i]], output_path, sep = "\t")
  }

  return(output_data)

}
