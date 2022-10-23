#' @title Use HPC information to generate the seed and output file name
#' @description Obtain the job array information from HPC
#' (e.g."SLURM_ARRAY_TASK_ID") to generate the seed and the output file name.
#' @param outfn_pre the first part of the output file name.
#' The second part is the seed to be used.
#' @param seed the "default" seed to be used. Default to 1234. The specific seed
#' depends on \code{seedtype}.
#' @param seedtype how the seed should be set. Default to  \strong{"seed+ID"}, i.e., the
#' sum of \code{seed} and the task ID from HPC. \strong{"ID"}, use the HPC task ID as the seed
#'  directly.  \strong{"seed"}, use the \code{seed} directly as the seed (note: if multiple
#'  tasks are submitted simultaneously, the output file name may be the same).
#' @param ID_env the name of the system variable for task ID. Default to
#' "SLURM_ARRAY_TASK_ID".
#' @param MAX_env the name of the system variable for maximum task ID. Default
#' to "SLURM_ARRAY_TASK_MAX".
#' @param N_env the name of the system variable for the number of tasks. Default
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
                    ID_env="SLURM_ARRAY_TASK_ID",
                    MAX_env="SLURM_ARRAY_TASK_MAX",
                    N_env="SLURM_ARRAY_TASK_COUNT"){
  # obtain the array information
  ainfo <- arrayinfo(ID_env=ID_env, N_env=N_env, MAX_env=MAX_env)

  # get the job array task ID
  ID <- ainfo$ID
  message(sprintf("\nJob array ID: %d", ID))

  if (seedtype=="seed+ID") {
    thisseed <- ID + seed
    maxseed <- ainfo$MAX+seed
  } else if (seedtype=="ID") {
    thisseed <- ID
    maxseed <- ainfo$MAX
  } else if (seedtype=="seed") {
    if (ainfo$N>1){
      warning("Multiple jobs are submitted and the results may be overwriten.")
    }
    thisseed <- seed
    maxseed <- seed
  }
  message(sprintf("The seed for brm(): %d", thisseed))

  # output file name
  maxstr <- sprintf('%d', floor(log(maxseed, 10))+1)
  outfn <- sprintf(paste0('%s_%0', maxstr, 'd.rds'), outfn_pre, thisseed)
  message(sprintf('The output file name is "%s".\n', outfn))

  out <- list(seed = thisseed,
              outfn = outfn)

  return(out)
}
