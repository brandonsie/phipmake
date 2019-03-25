#' Convert BLAST+ alignment table to two outputs. First, a full symmetrical two
#' column table of aligning peptides. Second, a subset of the first, including
#' only intra-protein alignments, for which protein identity is specified by
#' an annotation file.
#'
#' @param blast.file directory path to blast file e.g. 1/PhageomeLa_000_BLAST.txt
#' @param annot Annotation file.
#' @param name1 file path & name for all-alignment output file tsv
#' @param name2 file path & name for intraprotein-alignment output file tsv
#'
#' @export



#blast.file PhageomeLa_000_BLAST.txt
#annot "D:/RData/PhIPdb/Annotation_Files/Peptide_Metadata/PhageomeLa_001/20190218_PhageomeLa_001_Universal.txt"
#name1 PhageomeLa_000_pairs.tsv
#name2 intra_pairs.tsv

# prepare_blast_pairs("D:/RData/PhIPdb/Annotation_Files/AllergomeL_001_BLAST.txt","D:/RData/PhIPdb/Annotation_Files/Peptide_Metadata/AllergomeL_001/20190218_AllergomeL_001_Universal.txt", "D:/RData/PhIPdb/Annotation_Files/AllergomeL_001_pairs.tsv","D:/RData/PhIPdb/Annotation_Files/AllergomeL_001_intrapairs.tsv")

#VirscanLar_000_BLAST.txt


prepare_blast_pairs <- function(blast.file, annot, name1, name2){

  library(magrittr)

  # Prepare First File

  #read only first two columns
  blast <- data.table::fread(blast.file, data.table = FALSE,
                             colClasses = c(rep("character", 2), rep("NULL", 10)))

  # make symmetrical
  blast2 <- cbind(blast[,2], blast[,1])
  pairs <- rbind(blast, blast2) %>% unique

  #write file
  data.table::fwrite(pairs, name1, sep = "\t")

  # Prepare Second File

  # one time preparation of intrapeptide polyclonal dictionary
  annot <- data.table::fread(annot, data.table = FALSE)


  intra_pairs <- pairs #start with all pairs and remove non-intra pairs
  all_proteins <- annot$pro_id %>% unique

  for(i in 1:length(all_proteins)){
    # for each protein
    # from annot, get all peptides of that protein
    # for each peptide in that protein
    # get aligning peptides
    # keep only intersect

    print(paste("Protein", i, "of", length(all_proteins), "length:", nrow(intra_pairs)))

    protein <- all_proteins[i]
    peptides <- annot$u_pep_id[annot$pro_id == protein]

    for(j in 1:length(peptides)){

      # print(paste("__peptide", j, "of", length(peptides), "length:", nrow(intra_pairs)))
      peptide <- peptides[j]
      aligning_peps <- intra_pairs[intra_pairs[,1] == peptide,2]

      # intra_alignments <- intersect(same_protein_peps, aligning_peps)
      non_intra_alignments <- setdiff(aligning_peps, peptides)
      if(length(non_intra_alignments) > 0){
        intra_pairs <- intra_pairs[!((intra_pairs[,1] == peptide & intra_pairs[,2] %in% non_intra_alignments) | (intra_pairs[,2] == peptide & intra_pairs[,1] %in% non_intra_alignments)),]
      }
    }
  }


  data.table::fwrite(intra_pairs,name2, sep = "\t")


}