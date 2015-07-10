#' Return Only Canonical Transcripts 
#' 
#' This function will return a data.frame of a SnpEff database dump file that
#' only contains the canonical transcripts
#' 
#' @param chr.filter Vector containing the chromosomes you want to keep in the 
#'        data.frame
#' @return A data.frame of a SnpEff database dump file that only contains the 
#'         canonical transcripts
#' @export
get_canonical_transcripts <- function(snpeff.dt) {

  message("Filtering for only transcript rows")
  snpeff.dt.cantrans <- 
    dplyr::filter_(snpeff.dt, .dots = list(~type == "Transcript"))

  snpeff.dt.cantrans <- 
    dplyr::filter_(snpeff.dt.cantrans, 
                   .dots = list(~cdsLength == canonicalTranscriptLength))

  snpeff.dt.cantrans <- 
    dplyr::group_by_(snpeff.dt.cantrans, .dots = list(~geneName))

  message("Keeping 5' more transcript for multiple canonical transcripts")
  # if multiples transcripts have the same longest length, then select the 
  # transcript that has the 5' more TSS
  snpeff.dt.cantrans <- 
    dplyr::arrange_(snpeff.dt.cantrans, .dots = list(~chr, ~start, ~end))

  snpeff.dt.cantrans.distinct <- 
    dplyr::distinct_(snpeff.dt.cantrans, .dots = list(~geneName))

  snpeff.dt.cantrans.distinct
}
