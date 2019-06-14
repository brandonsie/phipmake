#' write_drake_params
#'
#' write default values to drake_params.tsv for phipmake pipeline
#'
#' @param dir PhIP-seq project directory.
#' @param screen_name Identifier for multiplexed screen, e.g. phipseq_0001.
#' @param counts_filename Name of demultiplexed, merged reads file in screen directory.
#' @param counts_type Identifier for type of counts data to append in output file names.
#' @param foldchange_filename Name of demultiplexed, merged foldchange file in screen directory.
#' @param foldchange_type Identifier for type of counts data to append in output file names.
#' @param enrichment_filename Name of demultiplexed, merged enrichment file in screen directory.
#' @param enrichment_type Identifier for type of enrichment data to append in output file names.
#' @param enrichment_threshold Threshold enrichment value for determining hits.
#' @param metadata_path Directory path string to metadata folder containing peptide files and intrapeptide blast alignment files.
#' @param output_extension Extension for tab-separated output files.
#'
#' @export


write_drake_params <- function(dir,
                               screen_name,
                               counts_filename = "counts.csv",
                               counts_type = "Counts",
                               foldchange_filename = "fold_change.csv",
                               foldchange_type = "FoldChange",
                               enrichment_filename = "enrichment.csv",
                               enrichment_type = "Enrichment",
                               enrichment_threshold = 5,
                               metadata_path = "/data/hlarman1/PhIPdb/Metadata/PeptideLibraries",
                               output_extension = ".tsv"){

  params <- c("screen_name",
              "counts_filename", "counts_type",
              "foldchange_filename", "foldchange_type",
              "enrichment_filename", "enrichment_type",
              "enrichment_threshold",
              "metadata_path", "output_extension")

   value <- c(screen_name, counts_filename,
              counts_type,
              foldchange_filename, foldchange_type,
              enrichment_filename,
              enrichment_type, enrichment_threshold,
              metadata_path, output_extension)

  param_df <- data.frame(params, value)

  data.table::fwrite(param_df,paste0(dir,"/drake_params.tsv"), sep = "\t")
}
