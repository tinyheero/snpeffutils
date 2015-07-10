#' Return the interval 
#' 
#' This function will return a window of range.len upstream and downstream 
#' of every transcription start site int he snpeff.dt
#' 
#' @param snpeff.dt SnpEff database dump data.table loaded through the 
#'        load_snpeff_db_dump() function
#' @param range.len Integer value indicating the number of bases to include 
#'        up- and down-stream of the transcription start site
#'        
#' @return A data.table of contain the chr, upstream, downstream, and geneName
#'         of the around the TSS region
#' @export
get_TSS_window <- function(snpeff.dt, range.len) {

  if (is.null(range.len)) {
    stop("range.len is not specified")
  }

  snpeff.dt.filter <- 
    dplyr::filter_(snpeff.dt, .dots = list(~type == "Gene"))

  varval <- list(lazyeval::interp(~start - range.len),
                 lazyeval::interp(~start + range.len))
  varname <- c("upstream", "downstream")

  tss.window <- dplyr::mutate_(snpeff.dt.filter, 
                               .dots = setNames(varval, varname))

  tss.window <- dplyr::select_(tss.window, 
                               .dots = list(~chr, ~upstream, 
                                            ~downstream, ~geneName))

  tss.window
}
