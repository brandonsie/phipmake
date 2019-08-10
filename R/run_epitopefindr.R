#' Assemble hit files and call epitopefindr
#'
#' @export
#'

run_epitopefindr <- function(hits_foldchange, annotation_merged_df){

  hits_foldchange <- data.frame(hits_foldchange)
  annotation_merged_df <- data.frame(annotation_merged_df)

  for(i in 2:ncol(hits_foldchange)){

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

    if(length(pt_hits_pep_ids) > 0){

      k = 500
      if(length(pt_hits_pep_ids) > k){
        pt_hits_pep_ids <- pt_hits_pep_ids[1:k]
      }

      pt_hits_annot <- annotation_merged_df[
        match(pt_hits_pep_ids, annotation_merged_df$u_pep_id),
        c("annotation", "pep_aa")]
      colnames(pt_hits_annot) <- c("ID", "Seq")

      fasta_path <- paste0("epitopefindr/temp/", pt_id, ".fasta")

      output_path <- paste0("epitopefindr/", pt_id)
      epitopefindr::writeFastaAA(pt_hits_annot, fasta_path)

      msa_name <- paste0("msa_", pt_id, ".pdf")

      e <- simpleError("epitopefindr error")
      tryCatch(epitopefindr::epfind(fasta_path, output_path, name.msa = msa_name),
               error = function(e) e)

    }


  }

}