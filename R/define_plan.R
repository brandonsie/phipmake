#' define_plan
#'
#' Initialize and return drake_plan object. New for github drake 7.0+.
#'
#'
#'
#' @export
#'

define_plan <- function(){
  options(stringsAsFactors = FALSE)

  library(remotes)
  library(drake)
  library(dplyr)
  #(!) load packages elsewhere
  #(!) setup defineparams function with defaults


  # Character vectors for tidy evaluation (sublibary names, filenames, etc.)
  # need to read params up here? or can do at beginning of drake plan?

  # ============================================================================
  # Settings

  # Load parameters from drake_params.tsv
  params <- data.table::fread(file_in("drake_params.tsv"))
  # use_params <- c("screen_name", "counts_filename", "enrichment_filename",
  #                 "enrichment_type", "enrichment_threshold", "metadata_path",
  #                 "output_extension")
  # for(i in 1:use_params){assign(i, getparam(params, i))}

  screen_name <- getparam(params, "screen_name")
  counts_filename <- getparam(params, "counts_filename")
  counts_type <- getparam(params, "counts_type")
  enrichment_filename <- getparam(params, "enrichment_filename")
  enrichment_type <- getparam(params, "enrichment_type")
  enrichment_threshold <- getparam(params, "enrichment_threshold")
  metadata_path <- getparam(params, "metadata_path")
  output_extension <- getparam(params, "output_extension")


  # Establish sublibrary names
  if(file.exists(counts_filename)){
    temp.counts <- data.table::fread(counts_filename)
  } else{
    stop(paste("Counts file is missing. Are you in the right working directory? Currently looking for an enrichment file named", counts_filename, "in directory:", getwd()))
  }

  if(file.exists(enrichment_filename)){
    temp.enrich <- data.table::fread(enrichment_filename)
  } else{
    stop(paste("Enrichment file is missing. Are you in the right working directory? Currently looking for an enrichment file named", enrichment_filename, "in directory:", getwd()))
  }

  c.libs <- u_pep_id_to_libnames(temp.counts[,1])
  e.libs <- u_pep_id_to_libnames(temp.enrich[,1])

  c.libnames <- c.libs[[4]]
  e.libnames <- e.libs[[4]]
  libnames <- union(c.libnames, e.libnames)

  c.lib.base <- c.libs[[2]]
  e.lib.base <- e.libs[[2]]
  lib.base <- union(c.lib.base, e.lib.base)

  # Establish output file names
  sn.ext <- paste0(".", output_extension)
  sn.lib.c <- paste0("_", c.libnames)
  sn.lib.e <- paste0("_", e.libnames)
  sn.libdir.c <- paste0(c.libnames, "/")
  sn.libdir.e <- paste0(e.libnames, "/")
  sn.libdir <- paste0(libnames, "/")
  sn.annot <- paste0("_annotated")
  sn.a.ext <- paste0(sn.annot, sn.ext)

  sn.enrichment <- paste0(screen_name, "_", enrichment_type)
  names.enrichment.pan <- paste0(sn.enrichment, sn.ext)
  names.enrichment.pan.annot <- paste0(sn.enrichment, sn.a.ext)
  sn.enrichment.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type)
  names.enrichment.sub <- paste0(sn.enrichment.sub, sn.ext)
  names.enrichment.sub.annot <- paste0(sn.enrichment.sub, sn.a.ext)

  sn.enrichment.promax <- paste0(sn.enrichment, "_promax")
  names.enrichment.promax.pan <- paste0(sn.enrichment.promax, sn.ext)
  names.enrichment.promax.pan.annot <- paste0(sn.enrichment.promax, sn.a.ext)
  sn.enrichment.promax.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_promax")
  names.enrichment.promax.sub <- paste0(sn.enrichment.promax.sub, sn.ext)
  names.enrichment.promax.sub.annot <- paste0(sn.enrichment.promax.sub, sn.a.ext)

  sn.hits <- paste0(sn.enrichment, "_hits")
  names.hits.pan <- paste0(sn.hits, sn.ext)
  names.hits.pan.annot <- paste0(sn.hits, sn.a.ext)
  sn.hits.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_hits")
  names.hits.sub <- paste0(sn.hits.sub, sn.ext)
  names.hits.sub.annot <- paste0(sn.hits.sub, sn.a.ext)

  sn.polycl <- paste0(screen_name, "_polyclonal")
  names.polycl.pan <- paste0(sn.polycl, sn.ext)
  names.polycl.pan.annot <- paste0(sn.polycl, sn.a.ext)
  sn.polycl.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_polyclonal")
  names.polycl.sub <- paste0(sn.polycl.sub, sn.ext)
  names.polycl.sub.annot <- paste0(sn.polycl.sub, sn.a.ext)

  sn.counts <- paste0(screen_name, "_", counts_type)
  names.counts.pan <- paste0(sn.counts, sn.ext)
  names.counts.pan.annot <- paste0(sn.counts, sn.a.ext)
  sn.counts.sub <- paste0(sn.libdir.c, screen_name, sn.lib.c, "_", counts_type)
  names.counts.sub <- paste0(sn.counts.sub, sn.ext)
  names.counts.sub.annot <- paste0(sn.counts.sub, sn.a.ext)

  sn.counts.prosum <- paste0(sn.counts, "_prosum")
  names.counts.prosum.pan <- paste0(sn.counts.prosum, sn.ext)
  names.counts.prosum.pan.annot <- paste0(sn.counts.prosum, sn.a.ext)
  sn.counts.prosum.sub <- paste0(sn.libdir.c, screen_name, sn.lib.c, "_", counts_type, "_prosum")
  names.counts.prosum.sub <- paste0(sn.counts.prosum.sub, sn.ext)
  names.counts.prosum.sub.annot <- paste0(sn.counts.prosum.sub, sn.a.ext)

  # Sublibrary directories create
  for(i in sn.libdir){
    if(!dir.exists(i)) dir.create(i)
  }

  # ============================================================================
  # Plan

  plan <- drake::drake_plan(

    # Load parameters in plan environment
    params = data.table::fread(file_in("drake_params.tsv")),

    screen_name = getparam(params, "screen_name"),
    counts_filename = getparam(params, "counts_filename"),
    counts_type = getparam(params, "counts_type"),
    enrichment_filename = getparam(params, "enrichment_filename"),
    enrichment_type = getparam(params, "enrichment_type"),
    enrichment_threshold = getparam(params, "enrichment_threshold"),
    metadata_path = getparam(params, "metadata_path"),
    output_extension = getparam(params, "output_extension"),

    # --------------------------------------------------------------------------
    # Enrichment

    enrichment = target(
      data.table::fread(file_in(!!enrichment_filename))
    ), #10

    write_enrichment = target(
      write_data(enrichment, file_out(!!names.enrichment.pan))
    ),

    enrichment_sub = target(
      split_data(enrichment)
    ),

    write_enrichment_sub = target(
      write_data(enrichment_sub, file_out(!!names.enrichment.sub))
    ),

    enrichment_annotations = target(
      read_annot_list(enrichment_sub[[1]], !!metadata_path)
    ),

    enrichment_sub_annot = target(
      annotate_data(enrichment_sub, enrichment_annotations)
    ),
    write_enrichment_sub_annot = target(
      write_data(enrichment_sub_annot, file_out(!!names.enrichment.sub.annot))
    ),

    enrichment_annot = target(
      dplyr::bind_rows(enrichment_sub_annot[-1])
    ),

    write_enrichment_annot = target(
      write_data(enrichment_annot, file_out(!!names.enrichment.pan.annot))
    ), # 18


    # Enrichment Promax
    enrichment_sub_promax = target(
      compute_promax(enrichment_sub, enrichment_annotations)
    ),

    write_enrichment_sub_promax = target(
      write_data(enrichment_sub_promax, file_out(!!names.enrichment.promax.sub))
    ),

    enrichment_sub_promax_annot = target(
      annotate_data(enrichment_sub_promax, enrichment_annotations)
    ),

    write_enrichment_sub_promax_annot = target(
      write_data(enrichment_sub_promax_annot, file_out(!!names.enrichment.promax.sub.annot))
    ),

    enrichment_promax = target(
      dplyr::bind_rows(enrichment_sub_promax[-1])
    ),

    write_enrichment_promax = target(
      write_data(enrichment_promax, file_out(!!names.enrichment.promax.pan))
    ),

    enrichment_promax_annot = target(
      dplyr::bind_rows(enrichment_sub_promax_annot[-1])
    ),

    write_enrichment_promax_annot = target(
      write_data(enrichment_promax_annot, file_out(!!names.enrichment.promax.pan.annot))
    ), #26

    # Hits
    hits_sub = target(
      compute_hits(enrichment_sub, enrichment_threshold)
    ),

    write_hits_sub = target(
      write_data(hits_sub, file_out(!!names.hits.sub))
    ),

    hits_sub_annot = target(
      annotate_data(hits_sub, enrichment_annotations)
    ),

    write_hits_sub_annot = target(
      write_data(hits_sub_annot, file_out(!!names.hits.sub.annot))
    ),

    hits = target(
      dplyr::bind_rows(hits_sub[-1])
    ),

    write_hits = target(
      write_data(hits, file_out(!!names.hits.pan))
    ),

    hits_annot = target(
      dplyr::bind_rows(hits_sub_annot[-1])
    ),

    write_hits_annot = target(
      write_data(hits_annot, file_out(!!names.hits.pan.annot))
    ), #34

    # Polyclonal
    blast_pairs = target(
      read_pairs_list(enrichment_sub[[1]], !!metadata_path)
    ),

    polycl_sub = target(
      compute_polycl(hits_sub, enrichment_annotations, blast_pairs)
    ),

    write_polycl_sub = target(
      write_data(polycl_sub, file_out(!!names.polycl.sub))
    ),

    polycl_sub_annot = target(
      annotate_data(polycl_sub, enrichment_annotations)
    ),

    write_polycl_sub_annot = target(
      write_data(polycl_sub_annot, file_out(!!names.polycl.sub.annot))
    ),

    polycl = target(
      dplyr::bind_rows(polycl_sub[-1])
    ),

    write_polycl = target(
      write_data(polycl, file_out(!!names.polycl.pan))
    ),

    polycl_annot = target(
      dplyr::bind_rows(polycl_sub_annot[-1])
    ),

    write_polycl_annot = target(
      write_data(polycl_annot, file_out(!!names.polycl.pan.annot))
    ),


    # --------------------------------------------------------------------------
    # Counts
    counts = data.table::fread(file_in(!!counts_filename)),

    write_counts = target(
      write_data(counts, file_out(!!names.counts.pan))
    ),

    counts_sub = target(
      split_data(counts)
    ),

    write_counts_sub = target(
      write_data(counts_sub, file_out(!!names.counts.sub))
    ),

    counts_annotations = target(
      if(mean(counts_sub[[1]] == enrichment_sub[[1]]) == 1){
        enrichment_annotations
      } else{
        read_annot_list(counts_sub[[1]], !!metadata_path)
      }
    ),

    counts_sub_annot = target(
      annotate_data(counts_sub, counts_annotations),
    ),

    write_counts_sub_annot = target(
      write_data(counts_sub_annot, file_out(!!names.counts.sub.annot))
    ),

    counts_annot = target(
      dplyr::bind_rows(counts_sub_annot[-1])
    ),

    write_counts_annot = target(
      write_data(counts_annot, file_out(!!names.counts.pan.annot))
    ),

    # Counts Prosum
    counts_sub_prosum = target(
      compute_prosum(counts_sub, counts_annotations)
    ),

    write_counts_sub_prosum = target(
      write_data(counts_sub_prosum, file_out(!!names.counts.prosum.sub))
    ),

    counts_sub_prosum_annot = target(
      annotate_data(counts_sub_prosum, counts_annotations)
    ),

    write_counts_sub_prosum_annot = target(
      write_data(counts_sub_prosum_annot, file_out(!!names.counts.prosum.sub.annot))
    ),

    counts_prosum = target(
      dplyr::bind_rows(counts_sub_prosum[-1])
    ),

    write_counts_prosum = target(
      write_data(counts_prosum, file_out(!!names.counts.prosum.pan))
    ),

    counts_prosum_annot = target(
      dplyr::bind_rows(counts_sub_prosum_annot[-1])
    ),

    write_counts_prosum_annot = target(
      write_data(counts_prosum_annot, file_out(!!names.counts.prosum.pan.annot))
    )


  )






  return(plan)

}
