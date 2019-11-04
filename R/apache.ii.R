#Below is bult-in APACHE.II score table.
#' APACHE II score table
#' @description This function calculates the APACHE II score table
#' @usage
#' apache.ii(data, id = names(data)[1],
#' which = c('temp', 'map', 'hr', 'rr', 'aapo2', 'pao2', 'ph', 'hco3', 'sodium', 'potassium', 'creatinine', 'hct', 'wbc',
#' 'gcs', 'age', 'chronic'),
#' ...)
#' @param data
#' An object of class data.frame.
#' This should have either:
#'
#' (1) Matched names with the confounders listed in which. Each would serve as one confounder in the score table or
#'
#' (2) Matched orders with the confounders listed in which.
#'
#' @param id
#' Column name of column id in data serving as subject id. Default is the first one.
#' Set to NULL if the data doesn't have ID column.
#' @param which
#' Which confounders will be calculated.
#' @return A tibble that contains scores for each confounders and total score.
#' @seealso \link{summary.score_tbl}.
#' @references
#' Knaus WA, Draper EA, Wagner DP, Zimmerman JE.
#' APACHE II: a severity of disease classification system. Crit Care Med. 1985 Oct;13(10):818-29.
#' PMID: \href{https://www.ncbi.nlm.nih.gov/pubmed/3928249}{3928249}
#' @export
apache.ii <- ScoreTable(
  aliases = list(temp ='Temperature', map ='Maximum Aterial Pressure',
                 hr = 'Heart Rate', rr = 'Respiratory Rate', aapo2 = 'AaPO2',
                 pao2 = 'PaO2', ph = 'PH', hco3 = 'HCO3-', sodium = 'Sodium', potassium = 'Potassium',
                 creatinine = 'Creatinine', hct = 'HCT', wbc = 'White-blood cell',
                 gcs = 'Glasgow Comma Score', age = 'Age', chronic = 'Chronic'),
  custom_cases =
    list(
      temp = list(
        temp >= 41 | temp < 30 ~ 4,
        temp >= 39 | temp < 32 ~ 3,
        temp < 34 ~ 2,
        temp >= 38.5 | temp < 36 ~ 1,
        !is.na(temp) ~ 0
      ),
      map = list(
        map >= 160 | map < 50 ~ 4,
        map >= 130 ~ 3,
        map >= 110 | map < 70 ~ 2,
        !is.na(map) ~ 0
      ),
      hr = list(
        hr >= 180 | hr < 40 ~ 4,
        hr >= 140 | hr < 55 ~ 3,
        hr >= 110 | hr < 70 ~ 2,
        !is.na(hr) ~ 0
      ),
      rr = list(
        rr >= 50 | rr < 6 ~ 4,
        rr >= 35 ~ 3,
        rr < 10 ~ 2,
        rr >= 25 | rr < 12 ~ 1,
        !is.na(rr) ~ 0
      ),
      aapo2 = list(
        fio2 < .5 | is.na(fio2) ~ 0,
        aapo2 >= 500 ~ 5,
        aapo2 >= 350 ~ 3,
        aapo2 >= 200 ~ 2,
        !is.na(aapo2) ~ 0
      ),
      pao2 = list(
        fio2 >= .5 ~ 0,
        pao2 < 55 ~ 4,
        pao2 <= 60 ~ 3,
        pao2 <= 70 ~ 1,
        !is.na(pao2) ~ 0
      ),
      ph = list(
        ph >= 7.7 | ph < 7.15 ~ 4,
        ph >= 7.6 | ph < 7.25 ~ 3,
        ph < 7.33 ~ 2,
        ph >= 7.5 ~ 1,
        TRUE ~ 0
      ),
      hco3 = list(
        !is.na(ph) ~ 0,
        hco3 >= 52 | hco3 < 15 ~ 4,
        hco3 >= 41 | hco3 < 18 ~ 3,
        hco3 < 22 ~ 2,
        hco3 >= 32 ~ 1,
        !is.na(hco3) ~ 0
      ),
      sodium = list(
        sodium >= 180 | sodium <= 110 ~ 4,
        sodium >= 160 | sodium < 120 ~ 3,
        sodium >= 155 | sodium < 130 ~ 2,
        sodium >= 150 ~ 1,
        !is.na(sodium) ~ 0
      ),
      potassium = list(
        potassium >= 7 | potassium < 2.5 ~ 4,
        potassium >= 6 ~ 3,
        potassium < 3 ~ 2,
        potassium >= 5.5 | potassium < 3.5 ~ 1,
        !is.na(potassium) ~ 0
      ),
      creatinine = list(
        creatinine >= 3.5 ~ 4,
        creatinine >= 2 ~ 3,
        creatinine >= 1.5 | creatinine < .6 ~ 2,
        !is.na(creatinine) ~ 0
      ),
      hct = list(
        hct >= 60 | hct < 20 ~ 4,
        hct >= 50 | hct < 30 ~ 2,
        hct >= 46 ~ 1,
        !is.na(hct) ~ 0
      ),
      wbc = list(
        wbc >= 40 | wbc < 1 ~ 4,
        wbc >= 20 | wbc < 3 ~ 2,
        wbc >= 15 ~ 1,
        !is.na(wbc) ~ 0
      ),
      gcs = list(
        !is.na(gcs) ~ 15 - gcs
      ),
      age = list(
        age >= 75 ~ 6,
        age >= 65 ~ 5,
        age >= 55 ~ 3,
        age >= 45 ~ 2,
        TRUE ~ 0
      ),
      chronic = list(
        rowSums(liver, heart, lung, kidney) == 0 ~ 0,
        as.logical(emergency) ~ 5,
        as.logical(elective) ~ 2,
        sum(elective, emergency, na.rm = TRUE) == 0 ~ 5
      )
    )
)
