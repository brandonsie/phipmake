#' write_drake_params
#'
#' write default values to drake_params.tsv for phipmake pipeline
#'
#' @param dir PhIP-seq project directory.
#' @param screen_name Identifier for multiplexed screen, e.g. phipseq_0001.
#' @param counts_filename Name of demultiplexed, merged reads file in screen directory.
#' @param counts_type Identifier for type of counts data to append in output file names.
#' @param enrichment_filename Name of demultiplexed, merged enrichment file in screen directory.
#' @param enrichment_type Identifier for type of enrichment data to append in output file names.
#' @param enrichment_threshold Threshold enrichment value for determining hits.
#' @param metadata_path Directory path string to metadata folder containing peptide files and intrapeptide blast alignment files.
#' @param output_extension Extension for tab-separated output files.


write_drake_params <- function(dir,
                                screen_name,
                                counts_filename,
                                counts_type = "Counts",
                                enrichment_filename = "enrichment.tsv",
                                enrichment_type = "Enrichment",
                                enrichment_threshold = 5,
                                metadata_path = "/data/hlarman1/PhIPdb/Metadata/PeptideLibraries",
                                output_extension = ".tsv"){

  params <- data.frame(params = c("screen_name", "counts_filename",
                                  "counts_type", "enrichment_filename",
                                  "enrichment_type", "enrichment_threshold",
                                  "metadata_path", "output_extension"),
                       value = c(screen_name, counts_filename,
                                 counts_type, enrichment_filename,
                                 enrichment_type, enrichment_threshold,
                                 metadata_path, output_extension))

  data.table::fwrite(params,paste0(dir,"/drake_params.tsv"))
}