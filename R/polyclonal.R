#' For multilibrary hits data, identify libraries, and call library_polycl
#' for each.
#'
#' @param data List with first element a character vector of library names and
#' subsequent elements data frames of data for corresponding libraries.
#' @param annot Phiplist of annotation files.
#' @param pairs Phiplist of blast alignment pair files.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#' @param verbose Logical whether or not to print progress.
#' @param parallel Logical whether or not to use future and future.apply to parallelize individual library computations.
#'
#' @export

compute_polycl <- function(data, annot, pairs, method = "independence_filter", verbose = TRUE, parallel = FALSE){
  # check for proper annotation order
  if(!is.null(annot) & !is.null(pairs)){
    if(mean(names(annot) == names(data)) < 1){
      stop(paste("Error: annotate_data: annotation and data mismatch",
                 names(annot), ";",names(data)))
    } else if(mean(names(pairs) == names(data)) < 1){
      stop(paste("Error: annotate_data: pairs and data mismatch",
                 names(pairs), ";", names(data)))
    }
  }

  # prep output data list
  output_data <- list()
  libs <- names(data)

  # define function to be called by loop
  prepare_polycl <- function(data, annot, pairs, method, verbose, i){
    if(verbose) print(paste("Polyclonal:", libs[i],":",i,"of",length(libs)))
    library_polycl(data[[i]],  annot[[i]], pairs[[i]], method, verbose)

  }

  # run loop
  if(parallel){

    require(future)
    require(future.apply)

    # future_lapply(1:length(libs), )

  } else{
    for(i in 1:length(libs)){
      output_data[[i]] <- prepare_polycl(data, annot, pairs, method, verbose, i)
    }

  }

  names(output_data) <- names(data)
  return(output_data)

}


#' For one peptide library, identify proteins from annotation file, and then
#' call protein_polycl for each protein.
#'
#' @param data Data frame with one library's hits data
#' @param annot Annotation file.
#' @param pairs Data frame of BLAST aligning pairs within each protein.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#' @param verbose Logical whether or not to print progress.
#'
#' @export

library_polycl <- function(data, annot, pairs, method = "independence_filter", verbose = TRUE){

  pro_ids <- annot$pro_id[match(data[,1], annot$u_pep_id)]  %>% na.omit
  proteins <- pro_ids %>% unique

  #setup polyclonal table
  lib_polycl <- data.frame(matrix(nrow = length(proteins), ncol = ncol(data)))
  colnames(lib_polycl) <- c("pro_id", colnames(data)[-1])
  lib_polycl[,1] <- proteins

  #loop through this librarys proteins
  if(verbose){pb <- pbar(0, length(proteins))}
  for(i in 1:length(proteins)){
    if(verbose >= 2){print(i)}
    if(verbose){setTxtProgressBar(pb, i)}
    lib_polycl[i, -1] <- data[c(1:length(pro_ids))[
      pro_ids == proteins[i]], ] %>% na.omit %>% protein_polycl(pairs, method)
  }
  if(verbose) cat("\n")

  return(lib_polycl)
}



#' Take data frame input and calculate the polyclonal score in each column or row.
#'
#' @param data Data frame of hits from which to calculate polyclonal score. The first column should contain peptide names. Each subsequent column should represent data for one sample and each row should represent data for one peptide. All peptides in data should be from the same protein.
#' @param pairs 2 column table of aligning pairs that should not be double-counted for polyclonal score. For independence_filter method, this data should contain no redundancy e.g. a==b and b==a or self alignments e.g. a==a
#' @param margin Numeric 1 for row 2 for column.
#' @param method Character string signifying which scoring method to use for evaluation. independence_filter or old_method.
#'
#' @return polycl_scores; A numeric vector with length equal to the number of samples represented in the data parameter. One polyclonal score for each patient, collapsed from all input peptide hits data.
#'
#' @export
#'


protein_polycl <- function(data, pairs, margin = 2, method = "independence_filter"){
  options(stringsAsFactors = FALSE)
  # require(magrittr)

  peptides <- data[,1] #peptide names
  sub_pairs <- pairs[pairs[,1] %in% peptides,] #subset relevant blast alignment pairs
  polycl_scores <- rep(0, (ncol(data)-1))

  # get hits for that sample
  # check which hits align to each other & collapse

  # check for trivial cases
  if(nrow(data) == 0){
    warning("Warning: no peptide data input to protein_polycl. (i.e., the 'data' parameter has 0 rows).")

  } else if((nrow(sub_pairs) == 0) | (nrow(data) == 1)){
    # if none of this protein's peptides align to each other,
    # or if this protein only has one peptide, then
    # polyclonal score <- sum of peptide hits scores
    polycl_scores <- protein_prosum(data[,-1])

  } else{
    for(i in 2:ncol(data)){
      # loop to process each sample separately

      # identify which peptides were hits for current sample
      sample_data <- as.numeric(data[,i])
      hits <- peptides[sample_data == 1]

      if(length(hits) == 0){
        polycl_scores[i-1] <- 0

      } else if(method == "independence_filter"){

        polycl_scores[i-1] <- independence_filter(sub_pairs, hits)

      } else if(method == "conservative_blast"){
        # initialize a vector to keep track of which hits have either been
        # counted for polyclonal or blast align to another hit that has been
        # counted for polyclonal.

        explained_hits <- vector(mode = "character")
        polycl_scores[i-1] <- 0

        for(j in hits){
          if(!(j %in% explained_hits)){
            polycl_scores[i-1] %<>% + 1
            aligning_peptides <- sub_pairs[sub_pairs[,1] == j, 2]
            explained_hits %<>% c(aligning_peptides)
          }
        }

      } else stop("Error: invalid method supplied to protein_polycl.")

    }
  }

  return(polycl_scores) #note this returns values only, no protein name
}


#' Compute independent hits usingg network graphing.
#'
#' @param edge Alignment dictionary
#' @param vertex List of nodes to compute on.
#'
#' @export


independence_filter  = function(edge,vertex){ #independence filter that takes a dictionary (defined above) and a set of nodes and tells the minimal number of unique epitopes
  require(igraph)


  nodes = unlist(vertex)
  links_filtered = subset(edge,unlist(edge[,1]) %in% nodes)
  links_filtered = subset(links_filtered,links_filtered[,2] %in% nodes)
  if(dim(links_filtered)[1]!=0){

    net <- as.undirected(graph_from_data_frame(d=links_filtered,vertices=nodes, directed=F) )
    x = decompose.graph(net)
    x_1 = x[sapply(x,vcount)<30]
    x_1_sum  = sum(unlist(lapply(x_1,independence.number)))
    x_2 = x[sapply(x,vcount)>=30]
    temp = c()
    #x_2 = x
    if(length(x_2) >0){
      for(R in 1:length(x_2)){
        x_2_r = x_2[[R]]
        while(max(degree(x_2_r)>5)){

          toss = degree(x_2_r)==max(degree(x_2_r))
          x_2_r = delete_vertices(x_2_r, V(x_2_r)[toss][1])
        }
        x_l = decompose.graph(x_2_r)
        temp[R] = sum(unlist(lapply(x_l,independence.number)))
      }
    }
    return(sum(x_1_sum)+sum(temp))
  }
  if(dim(links_filtered)[1]==0){
    return(length(nodes))
  }
}
