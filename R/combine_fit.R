#' @title Combine multiple files of fit objects
#' @description combine the fit objects saved in separate files.
#'
#' @param path path to the fit objects; will be use as `path` in `list.files()`.
#' @param pattern wildcard used to identify the single fit objects; will be use
#' as `pattern` in `list.files()`.
#' @param outfn the output file name of the combined fit objects. If outfn is
#' `NULL` (default), no file will be saved locally.
#' @param bs whether perform bridge sampling and save the result. Default to FALSE.
#' @param ... other arguments to be used in `brms::bridge_sampler`.
#'
#' @return a combined fit object.
#' @export
#'
#' @examples
#'\dontrun{
#' combine_fit("brmfit*", outfn="combined_brmfit")
#' combine_fit("brmfit*", outfn="combined_brmfit", bs=TRUE)
#'}

combine_fit <- function(path=".", pattern, outfn=NULL, bs=FALSE, ...){
  # list all files
  filelist <- list.files(path, pattern, full.names=T)
  N_fit <- length(filelist)

  # read the first rds
  combined_fit <- readRDS(filelist[[1]])

  if (N_fit > 1) {
    for (i in 2:N_fit) {
      combined_fit <- brms::combine_models(combined_fit, readRDS(filelist[[i]]))
    }
  }

  # save the combined fit object if outfn is not NULL
  if (!is.null(outfn)){
    saveRDS(combined_fit, file = outfn)
  }
  message(sprintf("%d files of fitted objects were combined!", N_fit))

  # perform bridge sampling if needed
  if (bs) {
    message(sprintf("\nBridge sampling for %s...", outfn))

    bs_fit <- brms::bridge_sampler(combined_fit, ...)

    if (!is.null(outfn)){
      saveRDS(bs_fit, file = paste0(substr(outfn,1,nchar(outfn)-4),"_bs.rds"))
    }
  }

  return(combined_fit)
}

