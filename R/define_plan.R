#' define_plan
#'
#' Initialize and return drake_plan object
#'
#' @param md_path Directory path to metadata peptide annotation files. Not implemented. need Drake wildcards.
#' @param pairs_path Directory path to intraprotein BLAST pairs files. Not implemented. need Drake wildcards.
#'
#' @export
#'

define_plan <- function(md_path, pairs_path){
  options(stringsAsFactors = FALSE)


  plan <- drake::drake_plan(

    # Settings
    strings_in_dots = FALSE,
    params = data.table::fread(file_in("drake_params.tsv")),
    screen_name = params$value[params$param == "screen_name"],
    mpath = params$value[params$param == "md_path"],
    ppath = params$value[params$param == "pairs_path"],
    enrichment_threshold = params$value[params$param == "enrichment_threshold"],

    # Counts
    counts = data.table::fread(file_in("counts.tsv"), data.table = FALSE),
    counts_sublibrary = split_data(counts, "counts"),
    counts_sublibrary_annotated =
      annotate_data(counts_sublibrary, "counts", md_path = mpath),
    counts_annotated =
      merge_data(counts_sublibrary_annotated, file_out("counts_annotated.tsv")),

    # (!) Counts Prosum
    counts_sublibrary_prosum = compute_prosum(counts_sublibrary, "prosum", md_path = mpath),
    counts_sublibrary_prosum_annotated =
      annotate_data(counts_sublibrary_prosum, "prosum", md_path = mpath),
    counts_prosum = merge_data(counts_sublibrary_prosum, file_out("counts_prosum.tsv")),
    counts_prosum_annotated =
      merge_data(counts_sublibrary_prosum_annotated, file_out("counts_prosum_annotated.tsv")),

    # Counts Promax (!) remove?
    counts_sublibrary_promax = compute_promax(counts_sublibrary, "promax", md_path = mpath),
    counts_sublibrary_promax_annotated =
      annotate_data(counts_sublibrary_promax, "promax", md_path = mpath),
    counts_promax = merge_data(counts_sublibrary_promax, file_out("counts_promax.tsv")),
    counts_promax_annotated =
      merge_data(counts_sublibrary_promax_annotated, file_out("counts_promax_annotated.tsv")),

    #(!) add scaled counts?

    # Enrichment
    enrichment =
      data.table::fread(file_in("enrichment.tsv"), data.table = FALSE),
    enrichment_sublibrary = split_data(enrichment, "enrichment"),
    enrichment_sublibrary_annotated =
      annotate_data(enrichment_sublibrary, "enrichment", md_path = mpath),
    enrichment_annotated =
      merge_data(enrichment_sublibrary_annotated, file_out("enrichment_annotated.tsv")),

    # Enrichment Promax
    enrichment_sublibrary_promax =
      compute_promax(enrichment_sublibrary, "enrichment", md_path = mpath),
    enrichment_sublibrary_promax_annotated =
      annotate_data(enrichment_sublibrary_promax, "enrichment", md_path = mpath),
    enrichment_promax =
      merge_data(enrichment_sublibrary_promax, file_out("enrichment_promax.tsv")),
    enrichment_promax_annotated =
      merge_data(enrichment_sublibrary_promax_annotated, file_out("enrichment_promax_annotated.tsv")),

    # Hits
    enrichment_sublibrary_hits = compute_hits(enrichment_sublibrary, enrichment_threshold),
    enrichment_sublibrary_hits_annotated =
      annotate_data(enrichment_sublibrary_hits, "hits", md_path = mpath),
    enrichment_hits = merge_data(enrichment_sublibrary_hits, file_out("hits.tsv")),
    enrichment_hits_annotated =
      merge_data(enrichment_sublibrary_hits_annotated, file_out("hits_annotated.tsv")),

    # Polyclonal
    enrichment_sublibrary_polycl =
      compute_polycl(enrichment_sublibrary_hits, "polyclonal", md_path = mpath, pairs_path = ppath),
    enrichment_sublibrary_polycl_annotated =
      annotate_data(enrichment_sublibrary_polycl, "polyclonal", md_path = mpath),
    enrichment_polyclonal =
      merge_data(enrichment_sublibrary_polycl, file_out("polyclonal.tsv")),
    enrichment_polyclonal_annotated =
      merge_data(enrichment_sublibrary_polycl_annotated, file_out("polyclonal_annotated.tsv"))
  )

  return(plan)
}