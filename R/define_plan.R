#' define_plan
#'
#' Initialize and return drake_plan object. New for github drake 7.0+.
#'
#' @param params_path Path to project parameters file that can be created using the write_drake_params function.
#'
#' @export
#'

define_plan <- function(params_path = "drake_params.tsv"){
  options(stringsAsFactors = FALSE)

  #(!) load packages elsewhere
  #(!) setup defineparams function with defaults


  # Character vectors for tidy evaluation (sublibary names, filenames, etc.)
  # need to read params up here? or can do at beginning of drake plan?

  # ============================================================================
  # Settings

  if(!file.exists(params_path)){
    warning(paste("Params file missing. If you haven't yet made a parameters file for this run, try write_drake_params(). Curently looking for a parameters file named", params_path, "in directory:", getwd()))
  }

  # Load parameters from drake_params.tsv
  params <- data.table::fread(file_in(params_path))
  # use_params <- c("screen_name", "counts_filename", "enrichment_filename",
  #                 "enrichment_type", "enrichment_threshold", "metadata_path",
  #                 "output_extension")
  # for(i in 1:use_params){assign(i, getparam(params, i))}

  screen_name <- phipmake::getparam(params, "screen_name")
  counts_filename <- phipmake::getparam(params, "counts_filename")
  counts_type <- phipmake::getparam(params, "counts_type")
  enrichment_filename <- phipmake::getparam(params, "enrichment_filename")
  enrichment_type <- phipmake::getparam(params, "enrichment_type")
  enrichment_threshold <- phipmake::getparam(params, "enrichment_threshold")
  metadata_path <- phipmake::getparam(params, "metadata_path")
  output_extension <- phipmake::getparam(params, "output_extension")


  # Establish sublibrary names
  if(file.exists(counts_filename)){
    temp.counts <- data.table::fread(counts_filename)
    c.libs <- phipmake::u_pep_id_to_libnames(temp.counts[,1])
    c.lib.base <- c.libs[[2]]
    c.libnames <- c.libs[[4]]

    sn.lib.c <- paste0("_", c.libnames)
    sn.libdir.c <- paste0(c.libnames, "/")

    for(i in sn.libdir.c){if(!dir.exists(i)) dir.create(i)}

  } else{
    warning(paste("Counts file is missing. Are you in the right working directory? Currently looking for a counts file named", counts_filename, "in directory:", getwd()))
    sn.lib.c <- sn.libdir.c <- "NA"
  }

  if(file.exists(enrichment_filename)){
    temp.enrich <- data.table::fread(enrichment_filename)
    e.libs <- phipmake::u_pep_id_to_libnames(temp.enrich[,1])
    e.lib.base <- e.libs[[2]]
    e.libnames <- e.libs[[4]]

    sn.lib.e <- paste0("_", e.libnames)
    sn.libdir.e <- paste0(e.libnames, "/")

    # Sublibrary directories create
    for(i in sn.libdir.e){if(!dir.exists(i)) dir.create(i)
      }
  } else{
    warning(paste("Enrichment file is missing. Are you in the right working directory? Currently looking for an enrichment file named", enrichment_filename, "in directory:", getwd()))
    sn.lib.e <- sn.libdir.e <- "NA"
  }

  # Establish output file names

  sn.ext <- paste0(".", output_extension)
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

  sn.polycl <- paste0(sn.enrichment, "_polyclonal")
  names.polycl.pan <- paste0(sn.polycl, sn.ext)
  names.polycl.pan.annot <- paste0(sn.polycl, sn.a.ext)
  sn.polycl.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_polyclonal")
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





  # ============================================================================
  # Plan

  plan <- drake::drake_plan(

    # Load parameters in plan environment
    params = data.table::fread(file_in("drake_params.tsv")),

    screen_name = phipmake::getparam(params, "screen_name"),
    counts_filename = phipmake::getparam(params, "counts_filename"),
    counts_type = phipmake::getparam(params, "counts_type"),
    enrichment_filename = phipmake::getparam(params, "enrichment_filename"),
    enrichment_type = phipmake::getparam(params, "enrichment_type"),
    enrichment_threshold = phipmake::getparam(params, "enrichment_threshold"),
    metadata_path = phipmake::getparam(params, "metadata_path"),
    output_extension = phipmake::getparam(params, "output_extension"),

    # --------------------------------------------------------------------------
    # Enrichment

    enrichment = target(
      data.table::fread(file_in(!!enrichment_filename))
    ), #10

    write_enrichment = target(
      phipmake::write_data(enrichment, file_out(!!names.enrichment.pan))
    ),

    enrichment_sub = target(
      phipmake::split_data(enrichment)
    ),

    write_enrichment_sub = target(
      phipmake::write_data(enrichment_sub, file_out(!!names.enrichment.sub))
    ),

    enrichment_annotations = target(
      phipmake::read_annot_list(enrichment_sub[[1]], !!metadata_path)
    ),

    enrichment_sub_annot = target(
      phipmake::annotate_data(enrichment_sub, enrichment_annotations)
    ),
    write_enrichment_sub_annot = target(
      phipmake::write_data(enrichment_sub_annot, file_out(!!names.enrichment.sub.annot))
    ),

    enrichment_annot = target(
      dplyr::bind_rows(enrichment_sub_annot[-1])
    ),

    write_enrichment_annot = target(
      phipmake::write_data(enrichment_annot, file_out(!!names.enrichment.pan.annot))
    ), # 18


    # Enrichment Promax
    enrichment_sub_promax = target(
      phipmake::compute_promax(enrichment_sub, enrichment_annotations)
    ),

    write_enrichment_sub_promax = target(
      phipmake::write_data(enrichment_sub_promax, file_out(!!names.enrichment.promax.sub))
    ),

    enrichment_sub_promax_annot = target(
      phipmake::annotate_data(enrichment_sub_promax, enrichment_annotations)
    ),

    write_enrichment_sub_promax_annot = target(
      phipmake::write_data(enrichment_sub_promax_annot, file_out(!!names.enrichment.promax.sub.annot))
    ),

    enrichment_promax = target(
      dplyr::bind_rows(enrichment_sub_promax[-1])
    ),

    write_enrichment_promax = target(
      phipmake::write_data(enrichment_promax, file_out(!!names.enrichment.promax.pan))
    ),

    enrichment_promax_annot = target(
      dplyr::bind_rows(enrichment_sub_promax_annot[-1])
    ),

    write_enrichment_promax_annot = target(
      phipmake::write_data(enrichment_promax_annot, file_out(!!names.enrichment.promax.pan.annot))
    ), #26

    # Hits
    hits_sub = target(
      phipmake::compute_hits(enrichment_sub, enrichment_threshold)
    ),

    write_hits_sub = target(
      phipmake::write_data(hits_sub, file_out(!!names.hits.sub))
    ),

    hits_sub_annot = target(
      phipmake::annotate_data(hits_sub, enrichment_annotations)
    ),

    write_hits_sub_annot = target(
      phipmake::write_data(hits_sub_annot, file_out(!!names.hits.sub.annot))
    ),

    hits = target(
      dplyr::bind_rows(hits_sub[-1])
    ),

    write_hits = target(
      phipmake::write_data(hits, file_out(!!names.hits.pan))
    ),

    hits_annot = target(
      dplyr::bind_rows(hits_sub_annot[-1])
    ),

    write_hits_annot = target(
      phipmake::write_data(hits_annot, file_out(!!names.hits.pan.annot))
    ), #34

    # Polyclonal
    blast_pairs = target(
      phipmake::read_pairs_list(enrichment_sub[[1]], !!metadata_path)
    ),

    polycl_sub = target(
      phipmake::compute_polycl(hits_sub, enrichment_annotations, blast_pairs)
    ),

    write_polycl_sub = target(
      phipmake::write_data(polycl_sub, file_out(!!names.polycl.sub))
    ),

    polycl_sub_annot = target(
      phipmake::annotate_data(polycl_sub, enrichment_annotations)
    ),

    write_polycl_sub_annot = target(
      phipmake::write_data(polycl_sub_annot, file_out(!!names.polycl.sub.annot))
    ),

    polycl = target(
      dplyr::bind_rows(polycl_sub[-1])
    ),

    write_polycl = target(
      phipmake::write_data(polycl, file_out(!!names.polycl.pan))
    ),

    polycl_annot = target(
      dplyr::bind_rows(polycl_sub_annot[-1])
    ),

    write_polycl_annot = target(
      phipmake::write_data(polycl_annot, file_out(!!names.polycl.pan.annot))
    ),


    # --------------------------------------------------------------------------
    # Counts
    counts = data.table::fread(file_in(!!counts_filename)),

    write_counts = target(
      phipmake::write_data(counts, file_out(!!names.counts.pan))
    ),

    counts_sub = target(
      phipmake::split_data(counts)
    ),

    write_counts_sub = target(
      phipmake::write_data(counts_sub, file_out(!!names.counts.sub))
    ),

    counts_annotations = target(
      if(mean(counts_sub[[1]] == enrichment_sub[[1]]) == 1){
        enrichment_annotations
      } else{
        phipmake::read_annot_list(counts_sub[[1]], !!metadata_path)
      }
    ),

    counts_sub_annot = target(
      phipmake::annotate_data(counts_sub, counts_annotations),
    ),

    write_counts_sub_annot = target(
      phipmake::write_data(counts_sub_annot, file_out(!!names.counts.sub.annot))
    ),

    counts_annot = target(
      dplyr::bind_rows(counts_sub_annot[-1])
    ),

    write_counts_annot = target(
      phipmake::write_data(counts_annot, file_out(!!names.counts.pan.annot))
    ),

    # Counts Prosum
    counts_sub_prosum = target(
      phipmake::compute_prosum(counts_sub, counts_annotations)
    ),

    write_counts_sub_prosum = target(
      phipmake::write_data(counts_sub_prosum, file_out(!!names.counts.prosum.sub))
    ),

    counts_sub_prosum_annot = target(
      phipmake::annotate_data(counts_sub_prosum, counts_annotations)
    ),

    write_counts_sub_prosum_annot = target(
      phipmake::write_data(counts_sub_prosum_annot, file_out(!!names.counts.prosum.sub.annot))
    ),

    counts_prosum = target(
      dplyr::bind_rows(counts_sub_prosum[-1])
    ),

    write_counts_prosum = target(
      phipmake::write_data(counts_prosum, file_out(!!names.counts.prosum.pan))
    ),

    counts_prosum_annot = target(
      dplyr::bind_rows(counts_sub_prosum_annot[-1])
    ),

    write_counts_prosum_annot = target(
      phipmake::write_data(counts_prosum_annot, file_out(!!names.counts.prosum.pan.annot))
    )


  )






  return(plan)

}
