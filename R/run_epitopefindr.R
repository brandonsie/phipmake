#' Assemble hit files and call epitopefindr
#'
#' @export
#'

run_epitopefindr <- function(hits_foldchange, annotation_merged_df){

  for(i in 2:ncol(hits_foldchange)){
    print(i)

    pt_id <- colnames(hits_foldchange)[i]
    pt_hits_pep_ids <- hits_foldchange[,1][
      order(hits_foldchange[,i][hits_foldchange[,i] > 1])]
    pt_hits_pep_ids <- pt_hits_pep_ids %>% as.matrix %>% as.character

    if(length(pt_hits_pep_ids) > 2000){
      pt_hits_pep_ids <- pt_hits_pep_ids[1:2000]
    }

    # annotate pep_ids and get pep_aa
    # annotation default is gene name, tile, and species. 50 char limit.
    # for each hits list, get pep_aa and start epitopefindr
    # if no hits, don't write

    if(length(pt_hits_pep_ids) > 0){
      pt_hits_annot <- annotation_merged_df[
        c("annotation", "pep_aa"),
        match(pt_hits_pep_ids, annotation_merged_df$u_pep_id)]
      colnames(pt_hits_annot) <- c("ID", "Seq")

      fasta_path <- paste0("epitopefindr/temp/", pt_id, ".fasta")
      output_path <- paste0("epitopefindr/", pt_id)
      epitopefindr::writeFastaAA(pt_hits_annot, fasta_path)
      epitopefindr::epfind(fasta_path, output_path)

    }


  }

}