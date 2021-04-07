#' Process biomarkers data
#'
#'
#' @description
#' \code{process_biomarker} sets up biomarkers for hypothesis testing
#'
#' @details
#' This function sets up biomarkers for hypothesis testing.
#' Updated: 2021-03-27
#'
#' @param biomarker_tbl         A tibble with columns,
#'                              c('person_id','start_date','measurement_date',
#'                              'end_date','measurement_concept_id',
#'                              'value_as_number')
#' @param biomarker_name        A string, e.g., "LDL cholesterol"
#' @param covariates_flagged    A tibble with columns,
#'                              c('person_id','dob','gender_concept_id',
#'                              'race_concept_id','start_date',
#'                              'first_drug_exposure','final_end_date','age',
#'                              'drug_exposed','treatment_new_exposed')
#' @param baseline_cols         A vector, e.g.
#'                              c('person_id', 'start_date',
#'                              'first_drug_exposure', 'final_end_date')
#' @param inclusion_cols        A vector, e.g.,
#'                              c('person_id', 'measurement_date',
#'                              'measurement_concept_id', 'value_as_number')
#' @param concept_id_exclusions A vector, e.g., for LDL, c(2212451)
#' @export

process_biomarker <- function(biomarker_tbl,biomarker_name,covariates_flagged,baseline_cols,inclusion_cols,concept_id_exclusions,...) {

  #Select columns of interest
  biomarker_tbl <- biomarker_tbl %>%
    select(all_of(inclusion_cols)) %>%
    distinct()

  #Remove "measurement_concept_id", with values that do not make sense
  biomarker_tbl <- biomarker_tbl %>%
    filter(!(measurement_concept_id %in% concept_id_exclusions))

  #Identify baseline and treatment periods
  ##Calculate baseline median biomarker value
  baseline_biomarker <- covariates_flagged %>%
    select(all_of(baseline_cols))
  baseline_biomarker <- inner_join(baseline_biomarker,biomarker_tbl,by="person_id")
  baseline_biomarker <- baseline_biomarker %>%
    filter(measurement_date >= start_date & measurement_date <= first_drug_exposure)
  baseline_biomarker <- baseline_biomarker %>%
    group_by(person_id) %>%
    summarise(biomarker_baseline_value = median(value_as_number)) %>%
    arrange(desc(biomarker_baseline_value))
  ###Remove outliers
  b <- boxplot(baseline_biomarker$biomarker_baseline_value, plot = FALSE)
  lowerwhisker<-b$stats[1]
  upperwhisker<-b$stats[5]
  baseline_biomarker <- baseline_biomarker %>%
    filter(lowerwhisker < biomarker_baseline_value & biomarker_baseline_value < upperwhisker)

  ##Calculate treatment median biomarker value
  treatment_biomarker <- covariates_flagged %>%
    select(all_of(baseline_cols))
  treatment_biomarker <- inner_join(treatment_biomarker,biomarker_tbl,by="person_id")
  treatment_biomarker <- treatment_biomarker %>%
    mutate(
      thirty_days_treatment = first_drug_exposure + days(30)
    )
  treatment_biomarker <- treatment_biomarker %>%
    filter(thirty_days_treatment <= measurement_date & measurement_date <= final_end_date)
  treatment_biomarker <- treatment_biomarker %>%
    group_by(person_id) %>%
    summarise(biomarker_treatment_value = median(value_as_number)) %>%
    arrange(desc(biomarker_treatment_value))
  ###Remove outliers
  treatment_biomarker <- treatment_biomarker %>%
    filter(lowerwhisker < biomarker_treatment_value & biomarker_treatment_value < upperwhisker)

  #Label baseline and treatment
  merged_biomarker <- inner_join(baseline_biomarker, treatment_biomarker, by = "person_id")
  merged_biomarker <- merged_biomarker %>%
    mutate(
      biomarker_change = biomarker_treatment_value - biomarker_baseline_value,
      biomarker_name = biomarker_name
    )
}
