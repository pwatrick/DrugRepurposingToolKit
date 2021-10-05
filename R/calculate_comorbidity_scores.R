#' Calculates Elixhauser comorbidity scores
#'
#'
#' @description
#' \code{calculate_comorbidity_scores} calculates Elixhauser comorbidity scores.
#'
#' @details
#' This function calculates Elixhauser comorbidity scores.
#'
#' Updated: 2021-10-02
#'
#' @param clinical_results_sub      A tibble
#' @param r_i                   A tibble
#' @export
calculate_comorbidity_scores <- function(clinical_results_sub, r_i) {

  #Calculate baseline score
  baseline_icd <- inner_join(clinical_results_sub, r_i, by = "person_id") %>%
    filter(
      ##Keep only codes between start_date and first_drug_exposure
      condition_date >= start_date & condition_date <= first_drug_exposure
    ) %>%
    select(-c("start_date", "first_drug_exposure", "final_end_date"))

  ##Process baseline ICD10CM data
  baseline_icd10cm <- baseline_icd %>%
    mutate(
      code = str_replace_all(concept_code, "\\.", "")
    ) %>%
    filter(vocabulary_id == "ICD10CM")
  baseline_comorbidity_icd10cm <-
    comorbidity::comorbidity(x = baseline_icd10cm, id = "person_id",
                             code = "code", map = "elixhauser_icd10_quan",
                             assign0 = FALSE)
  baseline_comorbidity_icd10cm <- as_tibble(baseline_comorbidity_icd10cm) %>%
    pivot_longer(!person_id, names_to = "group", values_to = "value") %>%
    rename(icd10cm_value = value)

  ##Process ICD9CM data
  baseline_icd9cm <- baseline_icd %>%
    mutate(
      code = str_replace_all(concept_code, "\\.", "")
    ) %>%
    filter(vocabulary_id == "ICD9CM")
  baseline_comorbidity_icd9cm <-
    comorbidity::comorbidity(x = baseline_icd9cm, id = "person_id",
                             code = "code", map = "elixhauser_icd9_quan",
                             assign0 = FALSE)
  baseline_comorbidity_icd9cm <- as_tibble(baseline_comorbidity_icd9cm) %>%
    pivot_longer(!person_id, names_to = "group", values_to = "value") %>%
    rename(icd9cm_value = value)

  #Merge data
  baseline_comorbidity_merged <- full_join(baseline_comorbidity_icd9cm,
                                           baseline_comorbidity_icd10cm,
                                           by = c("person_id", "group")) %>%
    mutate(
      icd9cm_value = replace_na(icd9cm_value, 0),
      icd10cm_value = replace_na(icd10cm_value, 0)
    )
  baseline_comorbidity_merged <- baseline_comorbidity_merged %>%
    mutate(
      merged_value = if_else(icd9cm_value == 1 | icd10cm_value == 1,1,0)
    ) %>%
    select(person_id, group, merged_value) %>%
    distinct()
  df_elixhauser_scores_baseline <- inner_join(baseline_comorbidity_merged,
                                              DrugRepurposingToolKit::vw_weights,
                                              by = "group") %>%
    mutate(score = merged_value*weight) %>%
    distinct()
  clinical_results_sub_ids <- clinical_results_sub %>% select(person_id) %>% distinct()
  df_elixhauser_scores_baseline <- df_elixhauser_scores_baseline %>%
    group_by(person_id) %>%
    summarise(elix_score = sum(score)) %>%
    select(person_id, elix_score) %>%
    distinct()
  df_elixhauser_scores_baseline <- full_join(df_elixhauser_scores_baseline,
                                             clinical_results_sub_ids,
                                             by = "person_id") %>%
    mutate(
      elix_score = replace_na(elix_score, 0)
    ) %>%
    rename(baseline_elix_score = elix_score)

  #Calculate treatment score
  treatment_icd <- inner_join(clinical_results_sub, r_i, by = "person_id") %>%
    filter(
      ##Keep only codes between start_date and final_end_date
      condition_date >= start_date & condition_date <= final_end_date
    ) %>%
    select(-c("start_date", "first_drug_exposure", "final_end_date"))

  ##Process treatment ICD10CM data
  treatment_icd10cm <- treatment_icd %>%
    mutate(
      code = str_replace_all(concept_code, "\\.", "")
    ) %>%
    filter(vocabulary_id == "ICD10CM")
  treatment_comorbidity_icd10cm <-
    comorbidity::comorbidity(x = treatment_icd10cm, id = "person_id",
                             code = "code", map = "elixhauser_icd10_quan",
                             assign0 = FALSE)
  treatment_comorbidity_icd10cm <- as_tibble(treatment_comorbidity_icd10cm) %>%
    pivot_longer(!person_id, names_to = "group", values_to = "value") %>%
    rename(icd10cm_value = value)

  ##Process ICD9CM data
  treatment_icd9cm <- treatment_icd %>%
    mutate(
      code = str_replace_all(concept_code, "\\.", "")
    ) %>%
    filter(vocabulary_id == "ICD9CM")
  treatment_comorbidity_icd9cm <-
    comorbidity::comorbidity(x = treatment_icd9cm, id = "person_id",
                             code = "code", map = "elixhauser_icd9_quan",
                             assign0 = FALSE)
  treatment_comorbidity_icd9cm <- as_tibble(treatment_comorbidity_icd9cm) %>%
    pivot_longer(!person_id, names_to = "group", values_to = "value") %>%
    rename(icd9cm_value = value)

  #Merge data
  treatment_comorbidity_merged <- full_join(treatment_comorbidity_icd9cm,
                                            treatment_comorbidity_icd10cm,
                                            by = c("person_id", "group")) %>%
    mutate(
      icd9cm_value = replace_na(icd9cm_value, 0),
      icd10cm_value = replace_na(icd10cm_value, 0)
    )
  treatment_comorbidity_merged <- treatment_comorbidity_merged %>%
    mutate(
      merged_value = if_else(icd9cm_value == 1 | icd10cm_value == 1,1,0)
    ) %>%
    select(person_id, group, merged_value) %>%
    distinct()
  df_elixhauser_scores_treatment <- inner_join(treatment_comorbidity_merged,
                                               DrugRepurposingToolKit::vw_weights,
                                               by = "group") %>%
    mutate(score = merged_value*weight) %>%
    distinct()
  clinical_results_sub_ids <- clinical_results_sub %>% select(person_id) %>% distinct()
  df_elixhauser_scores_treatment <- df_elixhauser_scores_treatment %>%
    group_by(person_id) %>%
    summarise(elix_score = sum(score)) %>%
    select(person_id, elix_score) %>%
    distinct()
  df_elixhauser_scores_treatment <- full_join(df_elixhauser_scores_treatment,
                                              clinical_results_sub_ids,
                                              by = "person_id") %>%
    mutate(
      elix_score = replace_na(elix_score, 0)
    ) %>%
    rename(treatment_elix_score = elix_score)

  #Merge scores
  df_elixhauser_scores <- inner_join(df_elixhauser_scores_baseline, df_elixhauser_scores_treatment, by = "person_id")

  return(df_elixhauser_scores)
}
