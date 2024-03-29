#' Assemble hit files and call epitopefindr
#'
#'
#' @export
#'

run_epitopefindr <- function(hits_foldchange, annotation_merged_df,  epitopefindr_latex, parallel = FALSE, eth = 0.01, ...){

  hits_foldchange <- data.frame(hits_foldchange)
  annotation_merged_df <- data.frame(annotation_merged_df)

  if(parallel){
    # run Parallelized

    print("parallel")

    registerDoParallel(detectCores()-1)

    foreach(i = 2:ncol(hits_foldchange)) %dopar%{
      run_single_epitopefindr(i, hits_foldchange, annotation_merged_df, epitopefindr_latex, ethr = eth, ...)
    }

  } else {
    # run non-parallel for loop

    print("non-parallel")

    for(i in 2:ncol(hits_foldchange)){
      t <- tryCatch(
        run_single_epitopefindr(i, hits_foldchange, annotation_merged_df, epitopefindr_latex, ethr = eth, ...),
        error = function(e) e
      )

      print(t)
      if(inherits(t, "error")) next
    }

  }

} # end run_epitopefindr


run_single_epitopefindr <- function(i, hits_foldchange, annotation_merged_df, epitopefindr_latex,
                                    thresh = 500, ethr = 0.01){
  # Function to be called by foreach and or linear for loop

  pt_id <- colnames(hits_foldchange)[i]
  pt_hits_pep_ids <- hits_foldchange[,1][
    c(1:nrow(hits_foldchange))[hits_foldchange[,i] > 1]]
  pt_hits_foldchange <- hits_foldchange[,i][
    c(1:nrow(hits_foldchange))[hits_foldchange[,i] > 1]]

  pt_hits_pep_sorted <- pt_hits_pep_ids[order(-pt_hits_foldchange)]
  pt_hits_pep_sorted <- pt_hits_pep_sorted %>% as.matrix %>% as.character

  # annotate pep_ids and get pep_aa
  # annotation default is gene name, tile, and species. 50 char limit.
  # for each hits list, get pep_aa and start epitopefindr
  # if no hits, don't write

  if(length(pt_hits_pep_sorted) > 0){

    k = thresh
    if(length(pt_hits_pep_sorted) > k){
      pt_hits_pep_sorted <- pt_hits_pep_sorted[1:k]
    }

    pt_hits_annot <- annotation_merged_df[
      match(pt_hits_pep_sorted, annotation_merged_df$u_pep_id),
      c("annotation", "pep_aa")]
    colnames(pt_hits_annot) <- c("ID", "Seq")

    fasta_path <- paste0("epitopefindr/temp/", pt_id, ".fasta")

    output_path <- paste0("epitopefindr/", pt_id)
    epitopefindr::writeFastaAA(pt_hits_annot, fasta_path)

    msa_name <- paste0("msa_", pt_id, ".pdf")

    if(epitopefindr_latex){
      epitopefindr::epfind(fasta_path, output_path, name.msa = msa_name, pdflatex = TRUE, e.thresh = ethr)
    } else{
      epitopefindr::epfind(fasta_path, output_path, name.msa = msa_name, pdflatex = FALSE, e.thresh = ethr)
    }
  }
}