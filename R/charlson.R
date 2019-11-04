#Below is bult-in Charlson score table.
#' Charlson score table
#' @description This function calculation the Charlson table
#' @usage
#' apache.ii(data, id = names(data)[1],
#' which = c('myocardial_infarct', 'congestive_heart_failure', 'peripheral_vascular_disease',
#' cerebrovascular_disease', 'dementia', 'chronic_pulmonary_disease',
#' connective_tissue_disease', 'ulcer_disease', 'mild_liver_disease', 'diabetes',
#' hemiplegia', 'moderate_or_severe_renal_disease', 'diabetes_with_end_organ_damage', 'any_tumor',
#' moderate_or_severe_liver_disease', 'metastatic_solid_tumor', 'AIDS'),
#' ...)
#' @param data
#' An object of class data.frame.
#' This should have either:
#'
#' (1) Matched names with the confounders listed in which. Each would serve as one confounder in the score table or
#'
#' (2) Matched order with the confounders listed in which.
#'
#' @param id
#' Column name of column id in data serving as subject id. Default is the first one.
#' Set to NULL if the data doesn't have ID column.
#' @param which
#' Which confounders will be calculated.
#' @return A tibble that contains scores for each confounders and total score.
#' @seealso \link{summary.score_tbl}.
#' @export
charlson =  ScoreTable(
  confounders = c('myocardial_infarct', 'congestive_heart_failure', 'peripheral_vascular_disease',
                  'cerebrovascular_disease', 'dementia', 'chronic_pulmonary_disease',
                  'connective_tissue_disease', 'ulcer_disease', 'mild_liver_disease', 'diabetes',
                  'hemiplegia', 'moderate_or_severe_renal_disease', 'diabetes_with_end_organ_damage', 'any_tumor',
                  'moderate_or_severe_liver_disease', 'metastatic_solid_tumor', 'AIDS'),
  scores = c(rep(1, 10), rep(2, 4), 3, 6, 6),
  aliases = c('Myocardial infarction', 'Congestive heart failure', 'Peripheral vascular disease',
              'Cerebrovascular disease', 'Dementia', 'Chronic pulmonary disease',
              'Connective tissue disease', 'Ulcer disease', 'Mild liver disease', 'Diabetes',
              'Hemiplegia', 'Moderate or severe renal disease', 'Diabetes with end organ damage', 'any tumor',
              'Moderate or severe liver disease', 'Metastatic solid tumor', 'AIDS')
)
