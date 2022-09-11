#' @title Use HPC information to generate the seed and output file name
#' @description Obtain the job array information from HPC
#' (e.g."SLURM_ARRAY_TASK_ID") to generate the seed and the output file name.
#' @param outfn_pre the first part of the output file name.
#' The second part is the seed to be used.
#' @param seed the "default" seed to be used. Default to 1234. The specific seed
#' depends on `seedtype`.
#' @param seedtype how the seed should be set. Default to "seed+ID", i.e., the
#' sum of `seed` and the task ID from HPC. "ID", use the HPC task ID as the seed
#'  directly. "seed", use the `seed` directly as the seed (note: if multiple
#'  tasks are submitted simultaneously, the output file name may be the same).
#' @param ID_str the name of the system variable for task ID. Default to
#' "SLURM_ARRAY_TASK_ID".
#' @param MAX_str the name of the system variable for maximum task ID. Default
#' to "SLURM_ARRAY_TASK_MAX".
#' @param N_str the name of the system variable for the number of tasks. Default
#' to "SLURM_ARRAY_TASK_COUNT".
#'
#' @return a named list of information (length of 2). "seed" is a numeric value,
#' which can be used as the seed. "outfn" is the output file name.
#' @export
#'
#' @examples
#' Sys.setenv(SLURM_ARRAY_TASK_ID="5",
#'            SLURM_ARRAY_TASK_MAX="10",
#'            SLURM_ARRAY_TASK_COUNT="8")
#'
#' out <- jobinfo("fit", seed=2022)

jobinfo <- function(outfn_pre, seed=1234,
                      seedtype = "seed+ID",
                      ID_str="SLURM_ARRAY_TASK_ID",
                      MAX_str="SLURM_ARRAY_TASK_MAX",
                      N_str="SLURM_ARRAY_TASK_COUNT"){
  # get the job array task ID
  ID <- as.integer(Sys.getenv(ID_str))
  sprintf("Job array code: %d", ID)

  if (seedtype=="seed+ID") {
    thisseed <- ID + seed
    maxseed <- as.integer(Sys.getenv(MAX_str))+seed
  } else if (seedtype=="ID") {
    thisseed <- ID
    maxseed <- as.integer(Sys.getenv(MAX_str))
  } else if (seedtype=="seed") {
    if (as.integer(Sys.getenv(N_str))>1){
      warning("Multiple jobs are submitted and the results may be overwriten.")
    }
    thisseed <- seed
    maxseed <- seed
  }
  sprintf("The seed for brm(): %d", thisseed)

  # output file name
  maxstr <- sprintf('%d', floor(log(maxseed, 10))+1)
  outfn <- sprintf(paste0('%s_%0', maxstr, 'd.rds'), outfn_pre, thisseed)

  out <- list(seed = thisseed,
              outfn = outfn)

  return(out)
}
