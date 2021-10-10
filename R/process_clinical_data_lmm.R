#' Clean and process EHR data for linear mixed model analysis
#'
#'
#' @description
#' \code{process_clinical_data_lmm} clean and process EHR data for linear mixed model analysis.
#'
#' @details
#' This function cleans and processes clinical data from electronic health
#' record (EHR) databases that have adopted the Observational Medical Outcomes
#' Partnership (OMOP) Common Data Model (CDM) format, for linear mixed model analysis.
#'
#' Updated: 2021-10-10
#'
#' @param clinical_results      A tibble
#' @param r_i                   A tibble
#' @export
process_clinical_data_lmm <- function(clinical_results, r_i) {

  #Calculate age
  t_age <- clinical_results %>%
    select(person_id, start_date, first_drug_exposure, final_end_date) %>%
    inner_join((r_c %>% select(person_id, dob)), by = "person_id") %>%
    mutate(
      baseline_age = lubridate::time_length(lubridate::interval(dob, first_drug_exposure), "year"),
      treatment_age = lubridate::time_length(lubridate::interval(dob, final_end_date), "year")
    ) %>%
    select(person_id, baseline_age, treatment_age) %>%
    distinct()
  ##Normalize age
  baseline_age <- t_age %>%
    select(person_id, baseline_age) %>%
    mutate(group = "baseline") %>%
    rename(age = baseline_age)
  treatment_age <- t_age %>%
    select(person_id, treatment_age) %>%
    mutate(group = "treatment") %>%
    rename(age = treatment_age)
  merged_age <- bind_rows(baseline_age, treatment_age)
  merged_age$age2 <- scale(merged_age$age)[,1]

  #Calculate Elixhauser scores
  elix1 <- clinical_results %>%
    select(person_id, start_date, first_drug_exposure, final_end_date) %>%
    distinct()
  elix2 <- calculate_comorbidity_scores(elix1, r_i)
  ##Normalize scores
  baseline_elix <- elix2 %>%
    select(person_id, baseline_elix_score) %>%
    mutate(group = "baseline") %>%
    rename(elix_score = baseline_elix_score)
  treatment_elix <- elix2 %>%
    select(person_id, treatment_elix_score) %>%
    mutate(group = "treatment") %>%
    rename(elix_score = treatment_elix_score)
  merged_elix <- bind_rows(baseline_elix, treatment_elix)
  merged_elix$elix_score2 <- scale(merged_elix$elix_score)[,1]

  #Prepare clinical results table for LMM
  baseline_biomarker <- clinical_results %>%
    select(person_id, biomarker_baseline_value) %>%
    mutate(group = "baseline") %>%
    rename(biomarker_value = biomarker_baseline_value)
  treatment_biomarker <- clinical_results %>%
    select(person_id, biomarker_treatment_value) %>%
    mutate(group = "treatment") %>%
    rename(biomarker_value = biomarker_treatment_value)
  merged_biomarker <- bind_rows(baseline_biomarker, treatment_biomarker)

  #Process gender and race variables
  t_gender_race <- clinical_results %>%
    mutate(
      race_not_white = if_else(race_concept_id == 8527, 0, 1),
      gender_female = if_else(gender_concept_id %in% c(8532, 45878463), 1, 0)
    )

  #Merge tables
  processed_data_lmm <- merged_age %>%
    inner_join(merged_elix, by = c("person_id", "group")) %>%
    inner_join(merged_biomarker, by = c("person_id", "group")) %>%
    inner_join(t_gender_race, by = "person_id") %>%
    mutate(
      exposed = if_else(group == "baseline", 0, 1)
    ) %>%
    select(person_id, group, exposed, age2, elix_score2, race_not_white, gender_female, biomarker_value) %>%
    distinct() %>%
    arrange(person_id)

  return(processed_data_lmm)
}
