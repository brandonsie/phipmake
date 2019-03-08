#' define_plan
#'
#' Initialize and return drake_plan object
#'
#' @param mpath Directory path to metadata peptide annotation files. Not implemented. need Drake wildcards.
#' @param ppath Directory path to intraprotein BLAST pairs files. Not implemented. need Drake wildcards.
#'
#' @export
#'

define_plan <- function(mpath, ppath){
  options(stringsAsFactors = FALSE)

  plan <- drake::drake_plan(

    # Settings
    strings_in_dots = FALSE,
    mpath = readLines("mpath.txt"),
    ppath = readLines("ppath.txt"),

    # Counts
    counts = data.table::fread(file_in("counts.tsv"), data.table = FALSE),
    counts_sublibrary = split_data(counts, "counts"),
    counts_sublibrary_annotated =
      annotate_data(counts_sublibrary, "counts", md_path = mpath),
    counts_annotated =
      merge_data(counts_sublibrary_annotated, "counts_annotated"),

    # Counts Promax
    counts_sublibrary_promax = compute_promax(counts_sublibrary, "promax", md_path = mpath),
    counts_sublibrary_promax_annotated =
      annotate_data(counts_sublibrary_promax, "promax", md_path = mpath),
    counts_promax = merge_data(counts_sublibrary_promax, "counts_promax"),
    counts_promax_annotated =
      merge_data(counts_sublibrary_promax_annotated, "counts_promax"),

    #(!) add scaled counts?

    # Enrichment
    enrichment =
      data.table::fread(file_in("enrichment.tsv"), data.table = FALSE),
    enrichment_sublibrary = split_data(enrichment, "enrichment"),
    enrichment_sublibrary_annotated =
      annotate_data(enrichment_sublibrary, "enrichment", md_path = mpath),
    enrichment_annotated =
      merge_data(enrichment_sublibrary_annotated, "enrichment_annotated"),

    # Enrichment Promax
    enrichment_sublibrary_promax =
      compute_promax(enrichment_sublibrary, "enrichment", md_path = mpath),
    enrichment_sublibrary_promax_annotated =
      annotate_data(enrichment_sublibrary_promax, "enrichment", md_path = mpath),
    enrichment_promax =
      merge_data(enrichment_sublibrary_promax, "enrichment_promax"),
    enrichment_promax_annotated =
      merge_data(enrichment_sublibrary_promax_annotated, "enrichment_promax"),

    # Hits
    enrichment_sublibrary_hits = compute_hits(enrichment_sublibrary),
    enrichment_sublibrary_hits_annotated =
      annotate_data(enrichment_sublibrary_hits, "hits", md_path = mpath),
    enrichment_hits = merge_data(enrichment_sublibrary_hits, "hits"),
    enrichment_hits_annotated =
      merge_data(enrichment_sublibrary_hits_annotated, "hits_annotated"),

    # Polyclonal
    enrichment_sublibrary_polycl =
      compute_polycl(enrichment_sublibrary_hits, "polyclonal", md_path = mpath, pairs_path = ppath),
    enrichment_sublibrary_polycl_annotated =
      annotate_data(enrichment_sublibrary_polycl, "polyclonal", md_path = mpath),
    enrichment_polyclonal =
      merge_data(enrichment_sublibrary_polycl, "polyclonal"),
    enrichment_polyclonal_annotated =
      merge_data(enrichment_sublibrary_polycl_annotated, "polyclonal_annotated")
  )

  return(plan)
}