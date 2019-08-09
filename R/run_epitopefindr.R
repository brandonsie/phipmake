#' Assemble hit files and call epitopefindr
#'
#' @export
#'

run_epitopefindr <- function(hits_foldchange, annotation_merged_df){

  for(i in 2:ncol(hits_foldchange)){

    pt_id <- colnames(hits_foldchange)[i]
    pt_hits_pep_ids <- hits_foldchange[,1][
      order(hits_foldchange[,i][hits_foldchange[,i] > 1])]
    pt_hits_pep_ids <- pt_hits_pep_ids %>% as.matrix %>% as.character

    print(paste("column", i, ", length", length(pt_hits_pep_ids)))


    # annotate pep_ids and get pep_aa
    # annotation default is gene name, tile, and species. 50 char limit.
    # for each hits list, get pep_aa and start epitopefindr
    # if no hits, don't write

    if(length(pt_hits_pep_ids) > 0){

      if(length(pt_hits_pep_ids) > 2000){
        pt_hits_pep_ids <- pt_hits_pep_ids[1:2000]
      }

      print(paste("dim", dim(annotation_merged_df)))
      print(paste("match", match(pt_hits_pep_ids, annotation_merged_df$u_pep_id)))

      pt_hits_annot <- annotation_merged_df[
        match(pt_hits_pep_ids, annotation_merged_df$u_pep_id),
        c("annotation", "pep_aa")]
      colnames(pt_hits_annot) <- c("ID", "Seq")

      fasta_path <- paste0("epitopefindr/temp/", pt_id, ".fasta")

      print(paste("path",fasta_path))

      output_path <- paste0("epitopefindr/", pt_id)
      epitopefindr::writeFastaAA(pt_hits_annot, fasta_path)

      e <- simpleError("epitopefindr error")
      tryCatch(epitopefindr::epfind(fasta_path, output_path), error = function(e) e)

    }


  }

}