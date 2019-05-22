#' Separate multilibrary peptide data into a phipmake list.
#'
#'
#' @param data Multilibrary data frame.
#'
#' @return List with n+1 elements for a data file with n sublibraries. The first
#' element is a character vector of sublibrary names in order. The subsequent
#' elements are data frames corresponding to the specified libraries, in
#' corresponding order.
#'
#' @export


split_data <- function(data){

  # identify library versions contained in data file
  libnames <- u_pep_id_to_libnames(data[,1])
  all.names <- libnames[[1]] # strsplit of u_pep_id
  libs.base <- libnames[[2]] # unique basenames (e.g. HumanLarma)
  libs.vers <- libnames[[3]] #versioned (e.g. HumanLarma_002, HumanLarma_003)
  libs.univ <- libnames[[4]] #mergednames if applicable (e.g. HumanLarma_000)

  #prepare output list
  output.data <- list()

  # library_versions (e.g. HumanLarma_002, HumanLarma_003) for each peptide
  all.vers <- paste(all.names[,1], all.names[,2], sep = "_")

  for(i in 1:length(libs.base)){
    # subset peptide data belonging to this base library
    lib <- libs.base[i]
    vers <- libs.vers[grep(lib, libs.vers)]
    sub.data <- data[all.vers %in% vers,]
    names(sub.data)[1] <- "u_pep_id" #coerce first column name to be proper
    # (!) split_data currently only works on peptide data with u_pep_id

    # add data to list for return output
    output.data[[i]] <- sub.data

    # # write data to sublibrary folder
    # if(!dir.exists(libs.univ[i])){dir.create(libs.univ[i])}
    # data.table::fwrite(sub.data, filename, sep = "\t", na = "NA")


  }

  names(output.data) <- libs.univ
  return(output.data)

}