#' @importFrom tibble tibble
NULL

#' dr_ts_tutorial_data
#'
#'
#' @format List with five objects:
#'          c("spredixcan_phenotype_ts", "cmap_genes",
#'          "ilincs_output", "ilincs_simvastatin_dm4740", "pres_drugs")
"dr_ts_tutorial_data"

#' drugsHyperlipidemia
#'
#' Known lipid-lowering drugs
#'
#' @format A data frame with 3 variables: \code{rxcui_in}, \code{drug_desc} and
#'   \code{indication}
#' @format A data frame with 134 rows and 3 variables:
#' \describe{
#'   \item{rxcui_in}{RxCUI ingredients}
#'   \item{drug_desc}{Name of active drug ingredient}
#'   \item{indication}{Drug indication}
#'   ...
#' }
#' @source MEDI-HPS (Wei W-Q, Cronin RM, Xu H, Lasko TA, Bastarache L, Denny JC.
#'         2013. Development and evaluation of an ensemble resource linking
#'         medications to their indications. J Am Med Inform Assoc 20:954–961.)
"drugsHyperlipidemia"

#' drugsHypertension
#'
#' Known antihypertensive drugs
#'
#' @format A data frame with 3 variables: \code{rxcui_in}, \code{drug_desc} and
#'   \code{indication}
#' @format A data frame with 134 rows and 3 variables:
#' \describe{
#'   \item{rxcui_in}{RxCUI ingredients}
#'   \item{drug_desc}{Name of active drug ingredient}
#'   \item{indication}{Drug indication}
#'   ...
#' }
#' @source MEDI-HPS (Wei W-Q, Cronin RM, Xu H, Lasko TA, Bastarache L, Denny JC.
#'         2013. Development and evaluation of an ensemble resource linking
#'         medications to their indications. J Am Med Inform Assoc 20:954–961.)
"drugsHypertension"

#' ddi_rxcui2in
#'
#' Map rxcuis to their rxcui ingredients
#'
#' @format A data frame with 3 variables: \code{rxcui_ingr}, \code{rxcui} and
#'   \code{rxcui_ingr_name}
"ddi_rxcui2in"

#' ddi_rxcui_names
#'
#' Map rxcuis to their names
#'
#' @format A data frame with 3 variables: \code{drug_name}, \code{vocabulary_id} and
#'   \code{rxcui}
"ddi_rxcui_names"

#' ddi_ohdsi_rxnorm
#'
#' Map rxcuis to their names
#'
#' @format A data frame with 2 variables: \code{concept_id}, \code{rxcui}
"ddi_ohdsi_rxnorm"

