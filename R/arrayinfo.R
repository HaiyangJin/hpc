
#' Obtain job ID and array information
#' @description Obtain job ID and array information in HPC (with SLURM, more see
#' [here](https://slurm.schedmd.com/job_array.html)).
#' @param ID_env the environment variable of the job array index value.
#' @param N_env the environment variable of the number of tasks in the job array
#' @param MAX_env the environment variable of the highest job array index value.
#' @param MIN_env the environment variable of the lowest job array index value
#' @param jobID_env the environment variable of the current job ID.
#' @param jobArrayID_env the environment variable of the first job ID of the array.
#' @param toint whether output the information as integer.
#'
#' @return the corresponding information.
#' @import dplyr
#' @export
#'
#' @examples
#' Sys.setenv("SLURM_ARRAY_TASK_ID"="123")
#' arrayinfo()$ID
arrayinfo <- function(ID_env="SLURM_ARRAY_TASK_ID",
                      N_env="SLURM_ARRAY_TASK_COUNT",
                      MAX_env="SLURM_ARRAY_TASK_MAX",
                      MIN_env="SLURM_ARRAY_TASK_MIN",
                      jobID_env="SLURM_JOB_ID",
                      jobArrayID_env="SLURM_ARRAY_JOB_ID",
                      toint=TRUE){

  # gather the information
  out <- data.frame(ID = Sys.getenv(ID_env),
                    N = Sys.getenv(N_env),
                    MAX = Sys.getenv(MAX_env),
                    MIN = Sys.getenv(MIN_env),
                    jobID = Sys.getenv(jobID_env),
                    jobArrayID = Sys.getenv(jobArrayID_env))

  # save as integers if needed
  if (toint) {
    out <- out |>
      dplyr::mutate(ID = as.integer(.data$ID),
                    N = as.integer(.data$N),
                    MAX = as.integer(.data$MAX),
                    MIN = as.integer(.data$MIN),
                    jobID = as.integer(.data$jobID),
                    jobArrayID = as.integer(.data$jobArrayID))
  }

  return(out)
}
