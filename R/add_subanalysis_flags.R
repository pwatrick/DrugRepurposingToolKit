#' Flag patients for subanalysis
#'
#'
#' @description
#' \code{add_subanalysis_flags} flags patients for subanalysis
#'
#' @details
#' This function flags patients for subanalysis, based on exposure to
#' known approved drugs for target phenotype. Used for excluding patients
#' exposed to known approved drugs.
#'
#' Updated: 2021-03-27
#'
#' @param indication_drug_exposed A tibble with columns,
#'                                c('person_id','rxcui_ingr','rxcui_ingr_name',
#'                                'group')
#' @param covariates              A tibble with columns,
#'                                c('person_id','dob', 'gender_concept_id',
#'                                'race_concept_id', 'start_date',
#'                                'first_drug_exposure', 'final_end_date','age')
#' @export

add_subanalysis_flags <- function(indication_drug_exposed, covariates,...) {

  treatment_exposed <- indication_drug_exposed %>% filter(group == "treatment")
  baseline_exposed <- indication_drug_exposed %>% filter(group == "baseline")
  treatment_exposed_new <- anti_join(treatment_exposed, baseline_exposed, by = c("person_id", "rxcui_ingr")) %>%
    arrange(person_id)

  #Flag patients from cohort with indication drug exposures during observation period with "1" = exposed; "0" = not exposed
  covariates <- covariates %>%
    mutate(
      drug_exposed = if_else((person_id %in% indication_drug_exposed$person_id),1,0),
      treatment_new_exposed = if_else((person_id %in% treatment_exposed_new$person_id),1,0)
    )
}
