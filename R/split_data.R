#' split_data
#'
#' Separate multilibrary peptide data into a a list of files for individual libraries.
#'
#'
#' @param data Multilibrary data frame.
#' @param file_root Filename to write.
#'
#' @export


split_data <- function(data, file_root){
  # take input multi-library data frame and return a list separated by library
  # also write sublibrary files to directory (!) will require name input param
  options(stringsAsFactors = FALSE)

  # only works on peptide data with u_pep_id

  # identify library versions contained in data file
  libnames <- u_pep_id_to_libnames(data[,1])
  all.names <- libnames[[1]] # strsplit of u_pep_id
  libs.base <- libnames[[2]] # unique basenames (e.g. HumanLarma)
  libs.vers <- libnames[[3]] #versioned (e.g. HumanLarma_002, HumanLarma_003)
  libs.univ <- libnames[[4]] #mergednames if applicable (e.g. HumanLarma_000)


  # write separate sub counts file for each sublibrary
  # and prepare to return list with data split by sublibrary

  # return list
  output.data <- list()
  output.data[[1]] <- libs.univ

  # library_versions (e.g. HumanLarma_002, HumanLarma_003) for each peptide
  all.vers <- paste(all.names[,1], all.names[,2], sep = "_")

  for(i in 1:length(libs.base)){
    # subset peptide data belonging to thise base library
    lib <- libs.base[i]
    vers <- libs.vers[grep(lib, libs.vers)]
    sub.data <- data[all.vers %in% vers,]
    names(sub.data)[1] <- "u_pep_id" #coerce first column name to be proper

    # write data to sublibrary folder
    if(!dir.exists(libs.univ[i])){dir.create(libs.univ[i])}
    sub.name <- paste0(libs.univ[i],"/",file_root,"_",libs.univ[i],".tsv")
    data.table::fwrite(sub.data, sub.name, sep = "\t")

    # add data to list for return output
    output.data[[i+1]] <- sub.data

  }

  return(output.data)

}