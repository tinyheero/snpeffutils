#' Load SnpEff Database Dump Text File
#' 
#' This function will wraps around the datatable::fread() function to allow for
#' faster and efficient reading of the SnpEff database dump files
#' 
#' @param dumpfile Text dumpfile from the SnpEff dump command
#' @return A data.table of a SnpEff database dump file 
#' @export
load_snpeff_db_dump <- function(dumpfile) {
  message("Loading dump file")
  snpEffDbDt <- data.table::fread(dumpfile, sep = "\t")
  snpEffDbDt
}
