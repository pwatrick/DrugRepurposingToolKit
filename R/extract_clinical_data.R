#' Extract clinical data from All of Us dataset
#'
#'
#' @description
#' \code{extract_clinical_data} creates SQL query to extract clinical EHR data
#' from All of Us dataset
#'
#' @details
#' Updated: 2021-03-27
#'
#' @param drug_concept_id      A string, indicating drug RxNorm ingredient
#'                             OHDSI concept_id, eg, "1332418" for amlodipine
#' @param biomarker_concept_id A string, indicating biomarker's OHDSI
#'                             concept_id, eg, "3004249" for Systolic Blood
#'                             Pressure
#' @param table_name           A string, indicating
#' @param table_type           A string, either
#'                             c("covariates", "drugs", "biomarkers")
#' @param indication_drug_concept_ids A string, with concept_ids for indication
#'                                    drugs, eg, for lipid-lowering drugs:
#'                                    "({drug_concept_id},1551803,19011061,
#'                                    1351115,1558242,19004048,19051463,
#'                                    19018419,1510813,1501617,1592085,1545958,
#'                                    1549686,46275447,1551860,19082770,1586226,
#'                                    19095309,46274162,957797,1517824,46287466,
#'                                    19022956,40165636,1518148,19026600,1539403
#'                                    ,1526475)"
#' @export

extract_clinical_data <- function(drug_concept_id, biomarker_concept_id, table_name,
                                  table_type, indication_drug_concept_ids) {

  if (table_type == "covariates") {

    sql_query <- glue::glue("
  WITH pw_codesets AS
  (SELECT s3.codeset_id, s3.concept_id, s3.concept_name
  FROM (

  (SELECT 1 as codeset_id, s1.concept_id, s1.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Drug') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {drug_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s1)

  UNION DISTINCT

  (SELECT 2 as codeset_id, s2.concept_id, s2.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Measurement') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {biomarker_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s2)) s3
),

pw_index_date AS
(
  SELECT s1.person_id,
         {drug_concept_id} as drug_concept_id,
         s1.drug_exposure_start_date
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s3.concept_name as drug_concept_name, s2.drug_exposure_start_date
      FROM (SELECT * FROM `drug_exposure`) s2
      INNER JOIN (SELECT concept_id, concept_name FROM pw_codesets WHERE codeset_id = 1) s3
      ON (s2.drug_concept_id = s3.concept_id))
  ) s1
),

pw_obsperiod AS
(
  SELECT s1.person_id,
         s1.drug_concept_id,
         min(s1.drug_exposure_start_date) as first_drug_exposure,
         max(s1.drug_exposure_start_date) as last_drug_exposure
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s2.drug_exposure_start_date
    FROM (SELECT * FROM pw_index_date) s2
    INNER JOIN (SELECT * FROM `visit_occurrence` WHERE visit_concept_id = 9202) s3
    ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
  ) s1
  GROUP BY s1.person_id, s1.drug_concept_id
),

pw_covariates AS
(
  SELECT s1.person_id,
         s1.dob,
         s1.gender_concept_id,
         s1.race_concept_id
  FROM (
    (SELECT s2.person_id, date(s2.birth_datetime) as dob, s2.gender_concept_id, s2.race_concept_id
      FROM (SELECT * FROM `person`) s2
      INNER JOIN (SELECT * FROM pw_index_date) s3
      ON (s2.person_id = s3.person_id)
      GROUP BY s2.person_id, s2.birth_datetime, s2.gender_concept_id, s2.race_concept_id)
  ) s1
),

pw_covariates_merged AS
(
  SELECT s1.person_id, s1.dob, s1.gender_concept_id, s1.race_concept_id, s1.drug_concept_id, s1.first_drug_exposure, s1.last_drug_exposure
  FROM (
    (SELECT s2.person_id, s2.dob, s2.gender_concept_id, s2.race_concept_id, s3.drug_concept_id, s3.first_drug_exposure, s3.last_drug_exposure
    FROM (SELECT * FROM pw_covariates) s2
    INNER JOIN (SELECT * FROM pw_obsperiod) s3
    ON (s2.person_id = s3.person_id)
    GROUP BY s2.person_id, s2.dob, s2.gender_concept_id, s2.race_concept_id, s3.drug_concept_id, s3.first_drug_exposure, s3.last_drug_exposure)
  ) s1
)
SELECT * FROM pw_covariates_merged;")
  }

  if (table_type == "drugs") {

    sql_query <- glue::glue("
WITH pw_codesets AS
(
  SELECT s3.codeset_id, s3.concept_id, s3.concept_name
  FROM (

  (SELECT 1 as codeset_id, s1.concept_id, s1.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Drug') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {drug_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s1)

  UNION DISTINCT

  (SELECT 2 as codeset_id, s2.concept_id, s2.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Measurement') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {biomarker_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s2)) s3
      ),
pw_index_date AS
(
  SELECT s1.person_id,
         {drug_concept_id} as drug_concept_id,
         s1.drug_exposure_start_date
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s3.concept_name as drug_concept_name, s2.drug_exposure_start_date
      FROM (SELECT * FROM `drug_exposure`) s2
      INNER JOIN (SELECT concept_id, concept_name FROM pw_codesets WHERE codeset_id = 1) s3
      ON (s2.drug_concept_id = s3.concept_id))
      ) s1
      ),
pw_obsperiod AS
(
  SELECT s1.person_id,
         s1.drug_concept_id,
         min(s1.drug_exposure_start_date) as first_drug_exposure,
         max(s1.drug_exposure_start_date) as last_drug_exposure
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s2.drug_exposure_start_date
    FROM (SELECT * FROM pw_index_date) s2
    INNER JOIN (SELECT * FROM `visit_occurrence` WHERE visit_concept_id = 9202) s3
    ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
  ) s1
  GROUP BY s1.person_id, s1.drug_concept_id
  ),
pw_start_end AS
(
  SELECT s1.person_id, s1.drug_concept_id, DATE_SUB(s1.first_drug_exposure, INTERVAL 12 MONTH) as start_date, DATE_ADD(s1.first_drug_exposure, INTERVAL 12 MONTH) as end_date
  FROM (SELECT * FROM pw_obsperiod) s1
),

pw_drugs AS
(
SELECT s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
FROM (
  (SELECT s2.person_id, s2.start_date, s2.end_date, s3.drug_concept_id, s3.drug_exposure_start_date
  FROM (SELECT * FROM pw_start_end) s2
  INNER JOIN (SELECT person_id, drug_concept_id, drug_exposure_start_date
              FROM `drug_exposure`
              WHERE drug_concept_id != 0) s3
  ON (s2.person_id = s3.person_id))
) s1
WHERE s1.drug_exposure_start_date >= s1.start_date
AND s1.drug_exposure_start_date <= s1.end_date
),
pw_drugs_out AS
(
SELECT s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
FROM (
  (SELECT s2.person_id, s2.start_date, s2.end_date, s2.drug_concept_id, s2.drug_exposure_start_date
  FROM (SELECT * FROM pw_drugs) s2
  INNER JOIN (SELECT * FROM `visit_occurrence` WHERE visit_concept_id = 9202) s3
  ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
) s1
GROUP BY s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
),
indication_drug_set AS
(
  SELECT t2.ancestor_concept_id, t2.descendant_concept_id, t3.concept_name AS descendant_concept_name
  FROM (
    (
      SELECT concept_id, concept_name
      FROM `concept`
      WHERE domain_id = 'Drug'
      AND concept_id IN {indication_drug_concept_ids}
    ) t1
    INNER JOIN
    (
      SELECT ancestor_concept_id, descendant_concept_id
      FROM `concept_ancestor`
    ) t2
    ON (t1.concept_id = t2.ancestor_concept_id)
    INNER JOIN
    (
      SELECT concept_id, concept_name
      FROM `concept`
      WHERE domain_id = 'Drug'
    ) t3
    ON (t2.descendant_concept_id = t3.concept_id)
       )
),
pw_drugs_selected AS
(
  SELECT s1.person_id, s1.start_date, s1.end_date, s1.drug_concept_id, s1.drug_exposure_start_date
  FROM (
    (
      SELECT s2.person_id, s2.start_date, s2.end_date, s2.drug_concept_id, s2.drug_exposure_start_date
      FROM (SELECT * FROM pw_drugs_out) s2
      INNER JOIN(SELECT * FROM indication_drug_set) s3
      ON (s2.drug_concept_id = s3.descendant_concept_id)
    )
      ) s1
)
SELECT po.person_id,
       po.start_date,
       po.end_date,
       po.drug_exposure_start_date,
       po.drug_concept_id
 FROM (SELECT * FROM pw_drugs_selected) po
group by po.person_id, po.start_date, po.end_date, po.drug_exposure_start_date, po.drug_concept_id;")
  }

  if (table_type == "biomarkers") {

    sql_query <- glue::glue("
WITH pw_codesets AS
(
  SELECT s3.codeset_id, s3.concept_id, s3.concept_name
  FROM (

  (SELECT 1 as codeset_id, s1.concept_id, s1.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Drug') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {drug_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s1)

  UNION DISTINCT

  (SELECT 2 as codeset_id, s2.concept_id, s2.concept_name
    FROM (
      SELECT t1.concept_id, t1.concept_name
      FROM (SELECT * FROM `concept` WHERE domain_id = 'Measurement') t1
      INNER JOIN (SELECT * FROM `concept_ancestor` WHERE ancestor_concept_id = {biomarker_concept_id}) t2
      ON (t1.concept_id = t2.DESCENDANT_CONCEPT_ID)
      GROUP BY t1.concept_id, t1.concept_name
      ) s2)) s3
      ),
pw_index_date AS
(
  SELECT s1.person_id,
         {drug_concept_id} as drug_concept_id,
         s1.drug_exposure_start_date
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s3.concept_name as drug_concept_name, s2.drug_exposure_start_date
      FROM (SELECT * FROM `drug_exposure`) s2
      INNER JOIN (SELECT concept_id, concept_name FROM pw_codesets WHERE codeset_id = 1) s3
      ON (s2.drug_concept_id = s3.concept_id))
      ) s1
      ),
pw_obsperiod AS
(
  SELECT s1.person_id,
         s1.drug_concept_id,
         min(s1.drug_exposure_start_date) as first_drug_exposure,
         max(s1.drug_exposure_start_date) as last_drug_exposure
  FROM (
    (SELECT s2.person_id, s2.drug_concept_id, s2.drug_exposure_start_date
    FROM (SELECT * FROM pw_index_date) s2
    INNER JOIN (SELECT * FROM `visit_occurrence` WHERE visit_concept_id = 9202) s3
    ON (s2.person_id = s3.person_id AND s2.drug_exposure_start_date = s3.visit_start_date))
  ) s1
  GROUP BY s1.person_id, s1.drug_concept_id
  ),
pw_start_end AS
(
  SELECT s1.person_id, s1.drug_concept_id, DATE_SUB(s1.first_drug_exposure, INTERVAL 12 MONTH) as start_date, DATE_ADD(s1.first_drug_exposure, INTERVAL 12 MONTH) as end_date
  FROM (SELECT * FROM pw_obsperiod) s1
),

pw_biomarker AS
(
  SELECT s1.person_id, s1.measurement_concept_id, s2.concept_name, s1.measurement_date, s1.value_as_number
  FROM (SELECT person_id, measurement_concept_id, measurement_date, value_as_number FROM `measurement`) s1
  INNER JOIN (SELECT * FROM pw_codesets WHERE codeset_id = 2) s2
  ON (s1.measurement_concept_id = s2.concept_id)
),

pw_biomarker_obs AS
(
  SELECT s1.person_id, s1.start_date, s1.measurement_date, s1.end_date, s1.measurement_concept_id, s1.value_as_number
  FROM (
    (SELECT s2.person_id, s2.start_date,  s3.measurement_date, s2.end_date, s3.measurement_concept_id, s3.value_as_number
    FROM (SELECT * FROM pw_start_end) s2
    INNER JOIN (SELECT person_id, measurement_concept_id, measurement_date, value_as_number
                FROM pw_biomarker
                WHERE value_as_number > 0
                AND value_as_number <= 250) s3
    ON (s2.person_id = s3.person_id))
  ) s1
  WHERE s1.measurement_date >= s1.start_date
  AND s1.measurement_date <= s1.end_date
),

pw_biomarker_out AS
(
  SELECT s1.person_id, s1.start_date, s1.measurement_date, s1.end_date, s1.measurement_concept_id, s1.value_as_number
  FROM (
    SELECT s2.person_id, s2.start_date, s2.measurement_date, s2.end_date, s2.measurement_concept_id, s2.value_as_number
    FROM (SELECT * FROM pw_biomarker_obs) s2
    INNER JOIN (SELECT * FROM `visit_occurrence` WHERE visit_concept_id = 9202) s3
    ON (s2.person_id = s3.person_id AND s2.measurement_date = s3.visit_start_date)
   ) s1
)
SELECT * FROM pw_biomarker_out;")
  }

  return(sql_query)
}
