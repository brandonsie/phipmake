#' Initialize and return drake_plan object. New for github drake 7.0+.
#'
#' @param params_path Path to project parameters file that can be created using the write_drake_params function.
#' @param runCounts Logical controlling whether or not to define counts targets.
#' @param runPairwise Logical controlling whether or not to define pairwise targets.
#' @param runFoldChange Logical controlling whether or not to define EdgeR fold change targets.
#' @param runEnrichment Logical controlling whether or not to define enrichment targets.
#' @param runPolyclonal Logical controlling whether or not to define enrichment targets.
#' @param runAVARDA Logical controlling whether or not to define AVARDA targets.
#'
#' @export
#'

define_plan <- function(
  params_path = "drake_params.tsv",
  runCounts = TRUE,
  runPairwise = FALSE,
  runFoldChange = TRUE,
  runEnrichment = TRUE,
  runHits = TRUE,
  runPolyclonal = TRUE,
  runAVARDA = FALSE){
  options(stringsAsFactors = FALSE)

  # ============================================================================
  # Settings

  if(!file.exists(params_path)){
    stop(paste("Params file missing.",
               "If you haven't yet made a parameters file for this run,",
               "try write_drake_params().",
               "Curently looking for a parameters file named",
               params_path, "in directory:", getwd()))
  }

  # Load parameters from drake_params.tsv
  params <- data.table::fread(file_in(params_path))
  # use_params <- c("screen_name", "counts_filename", "enrichment_filename",
  #                 "enrichment_type", "enrichment_threshold", "metadata_path",
  #                 "output_extension")
  # for(i in 1:use_params){assign(i, getparam(params, i))}

  screen_name <- getparam(params, "screen_name")
  counts_filename <- getparam(params, "counts_filename")
  counts_type <- getparam(params, "counts_type")
  foldchange_filename <- getparam(params, "foldchange_filename")
  foldchange_type <- getparam(params, "foldchange_type")
  enrichment_filename <- getparam(params, "enrichment_filename")
  enrichment_type <- getparam(params, "enrichment_type")
  # enrichment_threshold <- getparam(params, "enrichment_threshold") %>% as.numeric
  hits_filename <- getparam(params, "hits_filename")
  metadata_path <- getparam(params, "metadata_path")
  output_extension <- getparam(params, "output_extension")
  output_separator <- getparam(params, "output_separator")
  sublibrary_parallel <- as.logical(getparam(params, "sublibrary_parallel"))

  # counts_libs <- getparam(params, "counts_libs")
  # enrichment_libs <- getparam(params, "enrichment_libs")

  # Establish sublibrary names
  if(file.exists(counts_filename)){
    temp.counts <- data.table::fread(counts_filename)
    c.libs <- u_pep_id_to_libnames(temp.counts[,1])
    c.lib.base <- c.libs[[2]]
    c.libnames <- c.libs[[4]]

    sn.lib.c <- paste0("_", c.libnames)
    sn.libdir.c <- paste0(c.libnames, "/")

    for(i in sn.libdir.c){if(!dir.exists(i)) dir.create(i)}

  } else{
    sn.lib.c <- sn.libdir.c <- "NA"

    if(runCounts) {
      warning(paste("Counts file is missing.",
                  "Are you in the right working directory?",
                  "Currently looking for a counts file named",
                  counts_filename, "in directory:", getwd()))
    }
  }

  if(file.exists(enrichment_filename)){
    temp.enrich <- data.table::fread(enrichment_filename)
    e.libs <- u_pep_id_to_libnames(temp.enrich[,1])
    e.lib.base <- e.libs[[2]]
    e.libnames <- e.libs[[4]]

    sn.lib.e <- paste0("_", e.libnames)
    sn.libdir.e <- paste0(e.libnames, "/")

    # Sublibrary directories create
    for(i in sn.libdir.e){if(!dir.exists(i)) dir.create(i)
      }
  } else{
    sn.lib.e <- sn.libdir.e <- "NA"
    if(runEnrichment | runPolyclonal | runAVARDA){
      warning(paste("Enrichment file is missing.",
                    "Are you in the right working directory?",
                    "Currently looking for an enrichment file named",
                    enrichment_filename, "in directory:", getwd()))

    }
  }

  # ============================================================================
  # Establish output file names
  # ============================================================================

  sn.ext <- paste0(".", output_extension)
  sn.annot <- paste0("_annotated")
  sn.a.ext <- paste0(sn.annot, sn.ext)

  # Enrichment
  sn.enrichment <- paste0(screen_name, "_", enrichment_type)
  names.enrichment.pan <- paste0(sn.enrichment, sn.ext)
  names.enrichment.pan.annot <- paste0(sn.enrichment, sn.a.ext)
  sn.enrichment.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type)
  names.enrichment.sub <- paste0(sn.enrichment.sub, sn.ext)
  names.enrichment.sub.annot <- paste0(sn.enrichment.sub, sn.a.ext)

  # Promax
  sn.enrichment.promax <- paste0(sn.enrichment, "_promax")
  names.enrichment.promax.pan <- paste0(sn.enrichment.promax, sn.ext)
  names.enrichment.promax.pan.annot <- paste0(sn.enrichment.promax, sn.a.ext)
  sn.enrichment.promax.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_promax")
  names.enrichment.promax.sub <- paste0(sn.enrichment.promax.sub, sn.ext)
  names.enrichment.promax.sub.annot <- paste0(sn.enrichment.promax.sub, sn.a.ext)

  # Hits
  sn.hits <- paste0(sn.enrichment, "_hits")
  names.hits.pan <- paste0(sn.hits, sn.ext)
  names.hits.pan.annot <- paste0(sn.hits, sn.a.ext)
  sn.hits.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_hits")
  names.hits.sub <- paste0(sn.hits.sub, sn.ext)
  names.hits.sub.annot <- paste0(sn.hits.sub, sn.a.ext)

  # Polyclonal
  sn.polycl <- paste0(sn.enrichment, "_polyclonal")
  names.polycl.pan <- paste0(sn.polycl, sn.ext)
  names.polycl.pan.annot <- paste0(sn.polycl, sn.a.ext)
  sn.polycl.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", enrichment_type, "_polyclonal")
  names.polycl.sub <- paste0(sn.polycl.sub, sn.ext)
  names.polycl.sub.annot <- paste0(sn.polycl.sub, sn.a.ext)


  # Counts
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


  # AVARDA
  names.enrichment.avarda <- paste0(sn.enrichment.sub,
                                    "_avardanames", sn.ext)


  # Fold Change
  sn.foldchange <- paste0(screen_name, "_", foldchange_type)
  names.foldchange.pan <- paste0(sn.foldchange, sn.ext)
  names.foldchange.pan.annot <- paste0(sn.foldchange, sn.a.ext)
  sn.foldchange.sub <- paste0(sn.libdir.e, screen_name, sn.lib.e, "_", foldchange_type)
  names.foldchange.sub <- paste0(sn.foldchange.sub, sn.ext)
  names.foldchange.sub.annot <- paste0(sn.foldchange.sub, sn.a.ext)

  # ============================================================================
  # Plans

  # --------------------------------------------------------------------------
  # Parameters
  # --------------------------------------------------------------------------

  params_plan <- drake::drake_plan(

    # Load parameters in plan environment
    params = data.table::fread(file_in("drake_params.tsv")),

    screen_name = getparam(params, "screen_name"),
    counts_filename = getparam(params, "counts_filename"),
    counts_type = getparam(params, "counts_type"),
    enrichment_filename = getparam(params, "enrichment_filename"),
    enrichment_type = getparam(params, "enrichment_type"),
    # enrichment_threshold = getparam(params, "enrichment_threshold") %>% as.numeric,
    hits_filename = getparam(params, "hits_filename"),
    metadata_path = getparam(params, "metadata_path"),
    output_extension = getparam(params, "output_extension"),
    output_separator = getparam(params, "output_separator"),
    sublibrary_parallel = as.logical(getparam(params, "sublibrary_parallel"))

  )

  # --------------------------------------------------------------------------
  # Enrichment

  if(runEnrichment) {
    enrichment_plan <- drake::drake_plan(
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
        read_annot_list(names(enrichment_sub), !!metadata_path)
      ),

      enrichment_sub_annot = target(
        annotate_data(enrichment_sub, enrichment_annotations)
      ),
      write_enrichment_sub_annot = target(
        write_data(enrichment_sub_annot,
                             file_out(!!names.enrichment.sub.annot))
      ),

      enrichment_annot = target(
        dplyr::bind_rows(enrichment_sub_annot)
      ),

      write_enrichment_annot = target(
        write_data(enrichment_annot,
                             file_out(!!names.enrichment.pan.annot))
      ), # 18


      # Enrichment Promax
      enrichment_sub_promax = target(
        compute_promax(enrichment_sub, enrichment_annotations)
      ),

      write_enrichment_sub_promax = target(
        write_data(enrichment_sub_promax,
                             file_out(!!names.enrichment.promax.sub))
      ),

      enrichment_sub_promax_annot = target(
        annotate_data(enrichment_sub_promax, enrichment_annotations)
      ),

      write_enrichment_sub_promax_annot = target(
        write_data(enrichment_sub_promax_annot,
                             file_out(!!names.enrichment.promax.sub.annot))
      ),

      enrichment_promax = target(
        dplyr::bind_rows(enrichment_sub_promax)
      ),

      write_enrichment_promax = target(
        write_data(enrichment_promax,
                             file_out(!!names.enrichment.promax.pan))
      ),

      enrichment_promax_annot = target(
        dplyr::bind_rows(enrichment_sub_promax_annot)
      ),

      write_enrichment_promax_annot = target(
        write_data(enrichment_promax_annot,
                             file_out(!!names.enrichment.promax.pan.annot))
      ), #26

    )
  }

  if(runHits){
    hits_plan <- drake::drake_plan(
      hits = target(
        data.table::fread(file_in(!!hits_filename))
      ),

      # hits_sub = target(
      #   compute_hits(enrichment_sub, !!enrichment_threshold)
      # ),
      #

      hits_sub = target(
        split_data(enrichment)
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
        dplyr::bind_rows(hits_sub)
      ),

      write_hits = target(
        write_data(hits, file_out(!!names.hits.pan))
      ),

      hits_annot = target(
        dplyr::bind_rows(hits_sub_annot)
      ),

      write_hits_annot = target(
        write_data(hits_annot, file_out(!!names.hits.pan.annot))
      ) #34



    )
  }

  if(runPolyclonal){
    polyclonal_plan <- drake::drake_plan(
      # Polyclonal
      blast_pairs = target(
        read_pairs_list(names(enrichment_sub), !!metadata_path)
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
        write_data(polycl_sub_annot,
                             file_out(!!names.polycl.sub.annot))
      ),

      polycl = target(
        dplyr::bind_rows(polycl_sub)
      ),

      write_polycl = target(
        write_data(polycl, file_out(!!names.polycl.pan))
      ),

      polycl_annot = target(
        dplyr::bind_rows(polycl_sub_annot)
      ),

      write_polycl_annot = target(
        write_data(polycl_annot, file_out(!!names.polycl.pan.annot))
      )
    )
  }


  # --------------------------------------------------------------------------
  # Fold Change

  if(runFoldChange){

    foldchange_plan <- drake::drake_plan(
      foldchange = target(
        data.table::fread(file_in(!!foldchange_filename))
      ), #10

      write_foldchange = target(
        write_data(foldchange, file_out(!!names.foldchange.pan))
      ),

      foldchange_sub = target(
        split_data(foldchange)
      ),

      write_foldchange_sub = target(
        write_data(foldchange_sub, file_out(!!names.foldchange.sub))
      ),

      foldchange_annotations = target(
        read_annot_list(names(foldchange_sub), !!metadata_path)
      ),

      foldchange_sub_annot = target(
        annotate_data(foldchange_sub, foldchange_annotations)
      ),
      write_foldchange_sub_annot = target(
        write_data(foldchange_sub_annot,
                   file_out(!!names.foldchange.sub.annot))
      ),

      foldchange_annot = target(
        dplyr::bind_rows(foldchange_sub_annot)
      ),

      write_foldchange_annot = target(
        write_data(foldchange_annot,
                   file_out(!!names.foldchange.pan.annot))
      )
    )

    }

    # --------------------------------------------------------------------------
    # Counts
  if(runCounts) {
    counts_plan <- drake::drake_plan(
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
        # if(mean(counts_sub[[1]] == enrichment_sub[[1]]) == 1){
        #   enrichment_annotations
        # } else{
          read_annot_list(names(counts_sub), !!metadata_path)
        # }
      ),

      counts_sub_annot = target(
        annotate_data(counts_sub, counts_annotations),
      ),

      write_counts_sub_annot = target(
        write_data(counts_sub_annot,
                             file_out(!!names.counts.sub.annot))
      ),

      counts_annot = target(
        dplyr::bind_rows(counts_sub_annot)
      ),

      write_counts_annot = target(
        write_data(counts_annot,
                             file_out(!!names.counts.pan.annot))
      ),

      # Counts Prosum
      counts_sub_prosum = target(
        compute_prosum(counts_sub, counts_annotations)
      ),

      write_counts_sub_prosum = target(
        write_data(counts_sub_prosum,
                             file_out(!!names.counts.prosum.sub))
      ),

      counts_sub_prosum_annot = target(
        annotate_data(counts_sub_prosum, counts_annotations)
      ),

      write_counts_sub_prosum_annot = target(
        write_data(counts_sub_prosum_annot,
                             file_out(!!names.counts.prosum.sub.annot))
      ),

      counts_prosum = target(
        dplyr::bind_rows(counts_sub_prosum)
      ),

      write_counts_prosum = target(
        write_data(counts_prosum, file_out(!!names.counts.prosum.pan))
      ),

      counts_prosum_annot = target(
        dplyr::bind_rows(counts_sub_prosum_annot)
      ),

      write_counts_prosum_annot = target(
        write_data(counts_prosum_annot,
                             file_out(!!names.counts.prosum.pan.annot))
      )

    )
  }

  if(runAVARDA){
    avpath <- "/data/hlarman1/PhIPdb/Software/AVARDA/"
    avcase <- names.enrichment.avarda[grep("Virscan", names.enrichment.avarda)]
    avdf <- paste0(avpath, "bin2/my_df.csv")
    avtotal <- paste0(avpath, "bin2/total_probability_xr2.csv")
    avpairwise <- paste0(avpath, "bin2/unique_probabilities3.csv")
    avblast <- paste0(avpath, "bin2/VirScan_filtered_virus_blast_new.csv")

    virdir <- e.libnames[grep("Virscan", e.libnames)]
    avout <- paste0(virdir, "/AVARDA/")
    if(!dir.exists(avout)) dir.create(avout)

    AVARDA_plan <- drake::drake_plan(
      enrichment_sub_avardanames = target(
        prepare_avarda_names(enrichment_sub, enrichment_annotations)
      ),
      write_enrichment_sub_avardanames = target(
        write_data(enrichment_sub_avardanames,
                             file_out(!!names.enrichment.avarda))
      ),
      command = target(
        paste0("sbatch --export=case=",!!avcase,
               ",thresh=",!!enrichment_threshold,
               ",df=",!!avdf,
               ",total=",!!avtotal,
               ",pairwise=",!!avpairwise,
               ",blast=",!!avblast,
               ",out_path=",!!avout,
               ",out_name=",paste0(!!screen_name,"_"),
               " ", !!avpath, "AVARDA_BMS.sh")
      ),
      write_command = target(
        writeLines(command, file_out("AVARDA_command.txt"))
      ),
      runAVARDA = target(
        {
          file_in(!!names.enrichment.avarda)
          system(command)
        }
      )
    )
  }


  if(runPairwise){


    #(!) todo
    #convert pairs input.csv to samples.txt
    #take input paths.txt (?)
    # assemble counts with gather_samples
    # call Daniel Pairwise to generate enrichments
    # check if runenrichments. add those targetse.
    # assemble plan below
  }

  #-------------
  # setup main plan based on input parameters runCounts runEnrichment runAVARDA
  main_plan <- params_plan
  if(runCounts){main_plan %<>% rbind(counts_plan)}
  if(runFoldChange){main_plan %<>% rbind(foldchange_plan)}
  if(runEnrichment){main_plan %<>% rbind(enrichment_plan)}
  if(runHits){main_plan %<>% rbind(hits_plan)}
  if(runPolyclonal){main_plan %<>% rbind(polyclonal_plan)}
  if(runAVARDA){main_plan %<>% rbind(AVARDA_plan)}

  return(main_plan)
}
