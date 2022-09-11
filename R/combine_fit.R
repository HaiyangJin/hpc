#' @title Combine multiple files of fit objects
#' @description combine the fit objects saved in separate files.
#'
#' @param path path to the fit objects; will be use as path in list.files().
#' @param pattern wildcard used to identify the single fit objects; will be use
#' as pattern in list.files().
#' @param outfn the output file name of the combined fit objects. If outfn is
#' NULL (default), no file will be saved locally.
#' @param ... other arguments to be used in list.files().
#'
#' @return a combined fit object.
#' @export
#' @importFrom brms
#'
#' @examples
#'\dontrun{
#' combine_fit("brmfit*", outfn="combined_brmfit")
#'}

combine_fit <- function(path=".", pattern, outfn=NULL, ...){
  # list all files
  filelist <- list.files(path, pattern, full.names=T, ...)
  # read the first rds
  combined_fit <- readRDS(filelist[[1]])

  if (length(filelist) > 1) {
    for (i in 2:length(filelist)) {
      combined_fit <- brms::combine_models(combined_fit, readRDS(filelist[[i]]))
    }
  }

  # save the combined fit object if outfn is not NULL
  if (!is.null(outfn)){
    saveRDS(combined_fit, file = outfn)
  }

  return(combined_fit)
}

