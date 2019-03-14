#' read_pairs
#'
#' Read BLAST pairs file based on md path and library name.
#'
#' @param libname Library name.
#' @param pairs_path Pairs data directory.
#'
#' @export
#'

read_pairs <- function(libname, pairs_path){
  #prepare to load intrapeptide alignment pairs for specific library
  # print(paste("libname:", libname))
  # print(paste("pairs path:", pairs_path))
  pairs_files <- list.files(pairs_path)

  # specify annotation file path
  lib_pairs_name <- pairs_files[grep(libname,pairs_files)] %>% min
  # print(paste("lib_pairs_name", lib_pairs_name))
  lib_pairs_path <- paste(pairs_path, lib_pairs_name, sep = "/")
  # print(paste("lib pairs path", lib_pairs_path))

  lib_pairs_files <- list.files(lib_pairs_path)
  # print(paste("lib pairs files", lib_pairs_files))
  lib_pairs_names <- lib_pairs_files[intersect(
    grep(libname,lib_pairs_files), grep("_intra", lib_pairs_files))]
  # print(paste("lib pairs name", lib_pairs_names))

  lib_pairs_file_path <- paste(lib_pairs_path, lib_pairs_names, sep = "/")
  print(paste("lib pairs file path", lib_pairs_file_path))

  annot <- data.table::fread(lib_pairs_file_path, data.table = FALSE)
  return(annot)

}
