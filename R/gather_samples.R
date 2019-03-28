#' read in multiple input files and extract specified columns
#'
#' @param sources Character vector of filepaths to read in. All source files should contain the same type of data - enrichments, normalized read counts, etc.
#' @param col.names Character vector of columns to extract. Vector order should match desired order in output.
#' @param retain.first.col Logical whether to also include the furst column of the first specified source file. Often this column will contain peptide names of interest.
#'
#' @export


gather_samples <- function(sources, col.names, retain.first.col = TRUE){


  for(i in 1:length(sources)){

    this.source <- data.table::fread(sources[i], data.table = FALSE) #read source file

    if(i == 1){ #setup output table
      output_file <- data.frame(matrix(nrow = nrow(this.source), ncol = (length(col.names))))
      colnames(output_file) <- col.names


      if(retain.first.col){
        output_file <- cbind(this.source[,1], output_file)
        colnames(output_file)[1] <- colnames(this.source)[1]

      }
    } #end output table setup

    # retrieve data for relevant col.names

    output_file[, colnames(output_file) %in% colnames(this.source)] <- this.source[, match(colnames(output_file), colnames(this.source))]

  } #end loop

  # check for missing files
  if(sum(is.na(output_file) > 0)){
    warning("Warning: missing values in gather_samples output.")
  }

  return(output_file)

}