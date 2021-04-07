#' Process covariates data
#'
#'
#' #' @description
#' \code{process_covariates} processes covariates data
#'
#' @details
#' This function processes covariates data. It calculates age start start of observation period,
#' keeps only adult patients (between 18 and 90 years at start of observation period), and
#' default value is to use all patients regardless of ancestry.
#'
#' Updated: 2021-03-27
#'
#' @param covariates A tibble with columns, c('person_id','dob','gender_concept_id','race_concept_id')
#' @param df_drug_obsperiod A tibble with columns, c('person_id','start_date','first_drug_exposure','final_end_date')
#' @param european A binary value with 1 = only use european ancestry, and 0 = use entire cohort.
#' @export

process_covariates <- function(covariates, df_drug_obsperiod, european = 0,...) {

  #Inner join tables to get dob and start_date in one dataframe
  covariates <- inner_join(covariates, df_drug_obsperiod, by = "person_id")

  #Keep only adult patients
  covariates <- covariates %>%
    mutate(
      age = lubridate::time_length(lubridate::interval(dob, start_date), "year")
    ) %>%
    filter(age >= 18 & age < 90)

  #Keep only European ancestry patients
  if (european == 1) {
    covariates <- covariates %>%
      filter(race_concept_id == 8527)
  }
  return(covariates)
}
