#' Read annotation files based on library names and metadata path.
#'
#' @param libs Character vector of library names.
#' @param md_path Metadata directory as character string.
#'
#' @return phiplist of annotations. First element is character vector of
#' libraries from libs input. Remaining elements are corresponding annotation
#' files.
#'
#' @export



read_annot_list <- function(libs, metadata_path){

  # prep output data list
  output_data <- list()
  output_data[[1]] <- libs

  for(i in 1:length(libs)){
    output_data[[i+1]] <- read_annot(libs[i], metadata_path)
  }

  return(output_data)

}

#' Read annotation file based on md path and library name.
#'
#' @param libname Library name.
#' @param metadata_path Metadata directory.
#'
#' @export

read_annot <- function(libname, metadata_path){

  # prepare to load annotation file for specific library
  annot_files <- list.files(metadata_path)

  # specify annotation file path
  lib_annot_name <- annot_files[grep(libname,annot_files)] %>% min
  lib_md_path <- paste(metadata_path, lib_annot_name, sep = "/")

  lib_annot_files <- list.files(lib_md_path)
  lib_annot_names <- lib_annot_files[intersect(
    grep(libname,lib_annot_files), grep("_Universal", lib_annot_files))]

  # take universal file if it exists
  if(length(lib_annot_names) > 1){
    lib_annot_names <- lib_annot_names[grep("000", lib_annot_names)]
  }

  lib_annot_file_path <- paste(lib_md_path, lib_annot_names, sep = "/")

  # read annotation file & return
  annot <- data.table::fread(lib_annot_file_path, data.table = FALSE)
  annot <- replace(annot, is.na(annot), "NA")

  return(annot)
}
