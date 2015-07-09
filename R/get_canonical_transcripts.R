#' Return canonical transcripts 
#' 
#' This function will return a data.frame of a SnpEff database dump file that
#' only contains the canonical transcripts
#' 
#' @param dumpfile Text dumpfile from the SnpEff dump command
#' @param chr.filter Vector containing the chromosomes you want to keep in the 
#'        data.frame
#' @return A data.frame of a SnpEff database dump file that only contains the 
#'         canonical transcripts
#' @export
get_canonical_transcripts <- function(dumpfile, chr.filter = NULL) {

  message("Loading dump file")
  snpEffDbDt <- data.table::fread(dumpfile, sep = "\t")

  message("Filtering for only transcript rows")
  snpEffDbDt.cantrans <- 
    dplyr::filter_(snpEffDbDt, .dots = list(~type == "Transcript"))

  snpEffDbDt.cantrans <- 
    dplyr::filter_(snpEffDbDt.cantrans, 
                   .dots = list(~cdsLength == canonicalTranscriptLength))

  # perform chromosome filter if needed
  if (!is.null(chr.filter) ){
    message("Filtering transcripts by chr.filter")
    snpEffDbDt.cantrans <- 
      dplyr::filter_(snpEffDbDt.cantrans, .dots = list(~chr %in% chr.filter))
  }

  snpEffDbDt.cantrans <- 
    dplyr::group_by_(snpEffDbDt.cantrans, .dots = list(~geneName))

  message("Keeping 5' more transcript for multiple canonical transcripts")
  # if multiples transcripts have the same longest length, then select the 
  # transcript that has the 5' more TSS
  snpEffDbDt.cantrans <- 
    dplyr::arrange_(snpEffDbDt.cantrans, .dots = list(~chr, ~start, ~end))

  snpEffDbDt.cantrans.distinct <- 
    dplyr::distinct_(snpEffDbDt.cantrans, .dots = list(~geneName))

  snpEffDbDt.cantrans.distinct
}
