#' Return the interval.
#' 
#' This function will return a window that is upstream and downstream
#' of every transcription start site in the snpeff.dt object.
#'
#' This function takes into account the strand the gene is on. So a 
#' downstream.len will apply to strand directionality the gene is.
#' For instance, if gene A is on the - strand then the downstream.len applies
#' "upstream" to the end coordinate of the gene (i.e. final downstream will
#' be less than the TSS site). 
#
#' The final upstream and downstream coordinate columns are reported from a 5' 
#' respect regardless of what strand the gene is one (similar to how gene 
#' coordinates reported in general). The window includes the TSS in its range.
#
#' @param snpeff.dt SnpEff database dump data.table loaded through the 
#'        load_snpeff_db_dump() function.
#' @param upstream.len Integer value indicating the number of bases to include 
#'        upstream of the transcription start site.
#' @param downstream.len Integer value indicating the number of bases to include 
#'        downstream of the transcription start site.
#'        
#' @return A data.table of containing the chr, upstream, downstream, and 
#'         geneName of the around the TSS region.
#' @export
get_TSS_window <- function(snpeff.dt, upstream.len = 0, downstream.len = 2000) {

  snpeff.dt.filter <- 
    dplyr::filter_(snpeff.dt, .dots = list(~type == "Gene"))

  varval <- list()

  varval[["downstream"]] <- lazyeval::interp(
                              ~f(strand == 1, start + downstream.len,
                               f(strand == -1, end + upstream.len, NA)),
                              .values = list(f = as.name("ifelse"))
                            )

  varval[["upstream"]] <- lazyeval::interp(
                            ~f(strand == 1, start - upstream.len,
                             f(strand == -1, end - downstream.len, NA)),
                             .values = list(f = as.name("ifelse"))
                          )

  tss.window <- dplyr::mutate_(snpeff.dt.filter, .dots = varval)

  tss.window <- dplyr::select_(tss.window, 
                               .dots = list(~chr, ~upstream, 
                                            ~downstream, ~geneName, ~strand))
}
