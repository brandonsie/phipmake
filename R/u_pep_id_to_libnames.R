#' u_pep_id_to_libnames
#'
#' Take ids of the form HumanLarma_000_<sequence> and returns unique libraries
#'
#'
#' @param upepid Character vector of universal id's.
#'
#' @export

u_pep_id_to_libnames <- function(upepid){
  # take ids of the form HumanLarma_000_<sequence> and returns unique libraries

  lnames <- upepid %>% as.matrix %>% as.character %>% strsplit("_") %>% unlist %>% matrix(nrow = 3) %>% t
  libs.base <- unique(lnames[,1])
  libs.vers <- unique(paste(lnames[,1], lnames[,2], sep = "_"))

  libs.univ <- vector(mode = "character")
  for(i in 1:length(libs.base)){
    lib.base <- libs.base[i]
    lib.ver <- libs.vers[grep(lib.base, libs.vers)]

    if(length(lib.ver) == 1){
      libs.univ %<>% c(lib.ver)
    } else {libs.univ %<>% c(paste0(lib.base, "_000"))}
  }

  return(list(lnames, libs.base, libs.vers, libs.univ))

}
