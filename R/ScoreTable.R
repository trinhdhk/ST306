# Below are generators and helpers that create and manipulate ScoreTable objects

#' Score Table generator
#' @description A function to formally create an object of class Score Table.
#' @param confounders
#' For simple generation.
#' A character vector that declares derived components in the score table (1) or
#' a named list (2) whose names define the derived components in the score table and
#' whose values define their respective possible levels
#' @param scores
#' For simple generation.
#' A numeric vector that declares score for derived components using method (1) or
#' a named list whose names follow the derived component names defined in method (2) and
#' whose values define their respective possible scores
#' @param aliases
#' A named list that define a pretty representatives for defined confounders
#' following the structure of list(confounder = aliases).
#' Unmentioned confounders will be left intact.
#' @param custom_cases
#' A named list that define a sophisticated way to define confounders and scoring algorithm,
#' following the structure of list(name = list(formulas))
#' Each name is the name for derived confounders.
#' Each formula in each sub-list folllows the form of condition ~ score in a "specific to general" order.
#' This is based on \link[dplyr]{case_when}.
#' @return
#' An object of class ScoreTable.
#' When called with no data, this will print out the structure of the ScoreTable.
#' When called with data passed, this will return a data frame of class score_tbl.
#' @examples
#' charlson =  ScoreTable(
#'confounders = c('myocardial_infarct', 'congestive_heart_failure', 'peripheral_vascular_disease',
#'                'cerebrovascular_disease', 'dementia', 'chronic_pulmonary_disease',
#'                'connective_tissue_disease', 'ulcer_disease', 'mild_liver_disease', 'diabetes',
#'                'hemiplegia', 'moderate_or_severe_renal_disease', 'diabetes_with_end_organ_damage', 'any_tumor',
#'                'moderate_or_severe_liver_disease', 'metastatic_solid_tumor', 'AIDS'),
#'scores = c(rep(1, 10), rep(2, 4), 3, 6, 6),
#'aliases = c('Myocardial infarction', 'Congestive heart failure', 'Peripheral vascular disease',
#'            'Cerebrovascular disease', 'Dementia', 'Chronic pulmonary disease',
#'            'Connective tissue disease', 'Ulcer disease', 'Mild liver disease', 'Diabetes',
#'            'Hemiplegia', 'Moderate or severe renal disease', 'Diabetes with end organ damage', 'any tumor',
#'            'Moderate or severe liver disease', 'Metastatic solid tumor', 'AIDS')
#')
#'
#'apache.ii <- ScoreTable(
#'aliases = list(temp ='Temperature', map ='Maximum Aterial Pressure',
#'               hr = 'Heart Rate', rr = 'Respiratory Rate', aapo2 = 'AaPO2',
#'               pao2 = 'PaO2', ph = 'PH', hco3 = 'HCO3-', sodium = 'Sodium', potassium = 'Potassium',
#'               creatinine = 'Creatinine', hct = 'HCT', wbc = 'White-blood cell',
#'               gcs = 'Glasgow Comma Score', age = 'Age', chronic = 'Chronic'),
#'custom_cases =
#'  list(
#'    temp = list(
#'      temp >= 41 | temp < 30 ~ 4,
#'      temp >= 39 | temp < 32 ~ 3,
#'      temp < 34 ~ 2,
#'      temp >= 38.5 | temp < 36 ~ 1,
#'      !is.na(temp) ~ 0
#'    ),
#'    map = list(
#'      map >= 160 | map < 50 ~ 4,
#'      map >= 130 ~ 3,
#'      map >= 110 | map < 70 ~ 2,
#'      !is.na(map) ~ 0
#'    ),
#'    hr = list(
#'      hr >= 180 | hr < 40 ~ 4,
#'      hr >= 140 | hr < 55 ~ 3,
#'      hr >= 110 | hr < 70 ~ 2,
#'      !is.na(hr) ~ 0
#'    ),
#'    rr = list(
#'      rr >= 50 | rr < 6 ~ 4,
#'      rr >= 35 ~ 3,
#'      rr < 10 ~ 2,
#'      rr >= 25 | rr < 12 ~ 1,
#'      !is.na(rr) ~ 0
#'    ),
#'    aapo2 = list(
#'      fio2 < .5 | is.na(fio2) ~ 0,
#'      aapo2 >= 500 ~ 5,
#'      aapo2 >= 350 ~ 3,
#'      aapo2 >= 200 ~ 2,
#'      !is.na(aapo2) ~ 0
#'    ),
#'    pao2 = list(
#'      fio2 >= .5 ~ 0,
#'      pao2 < 55 ~ 4,
#'      pao2 <= 60 ~ 3,
#'      pao2 <= 70 ~ 1,
#'      !is.na(pao2) ~ 0
#'    ),
#'    ph = list(
#'      ph >= 7.7 | ph < 7.15 ~ 4,
#'      ph >= 7.6 | ph < 7.25 ~ 3,
#'      ph < 7.33 ~ 2,
#'    ph >= 7.5 ~ 1,
#'    TRUE ~ 0
#'   ),
#'  hco3 = list(
#'    !is.na(ph) ~ 0,
#'    hco3 >= 52 | hco3 < 15 ~ 4,
#'    hco3 >= 41 | hco3 < 18 ~ 3,
#'    hco3 < 22 ~ 2,
#'    hco3 >= 32 ~ 1,
#'    !is.na(hco3) ~ 0
#'  ),
#'  sodium = list(
#'    sodium >= 180 | sodium <= 110 ~ 4,
#'    sodium >= 160 | sodium < 120 ~ 3,
#'      sodium >= 155 | sodium < 130 ~ 2,
#'      sodium >= 150 ~ 1,
#'    !is.na(sodium) ~ 0
#'  ),
#'  potassium = list(
#'    potassium >= 7 | potassium < 2.5 ~ 4,
#'    potassium >= 6 ~ 3,
#'    potassium < 3 ~ 2,
#'    potassium >= 5.5 | potassium < 3.5 ~ 1,
#'    !is.na(potassium) ~ 0
#'  ),
#'  creatinine = list(
#'    creatinine >= 3.5 ~ 4,
#'    creatinine >= 2 ~ 3,
#'    creatinine >= 1.5 | creatinine < .6 ~ 2,
#'    !is.na(creatinine) ~ 0
#'  ),
#'  hct = list(
#'    hct >= 60 | hct < 20 ~ 4,
#'    hct >= 50 | hct < 30 ~ 2,
#'    hct >= 46 ~ 1,
#'    !is.na(hct) ~ 0
#'  ),
#'  wbc = list(
#'    wbc >= 40 | wbc < 1 ~ 4,
#'    wbc >= 20 | wbc < 3 ~ 2,
#'    wbc >= 15 ~ 1,
#'    !is.na(wbc) ~ 0
#'  ),
#'   gcs = list(
#'    !is.na(gcs) ~ 15 - gcs
#'  ),
#'  age = list(
#'    age >= 75 ~ 6,
#'    age >= 65 ~ 5,
#'    age >= 55 ~ 3,
#'    age >= 45 ~ 2,
#'    TRUE ~ 0
#'  ),
#'  chronic = list(
#'    rowSums(liver, heart, lung, kidney) == 0 ~ 0,
#'    as.logical(emergency) ~ 5,
#'    as.logical(elective) ~ 2,
#'    sum(elective, emergency, na.rm = TRUE) == 0 ~ 5
#'  )
#' )
#')
#' @seealso
#' \link[dplyr]{case_when}, \link{as.data.frame.ScoreTable}, \link{apache.ii}, \link{summary.score_tbl}
#' @export
ScoreTable <- function(confounders, scores, aliases = NULL, custom_cases){
  # First, we need to determine what we have
  # A type of binary will only have two levels, while categorical will have more than 2
  # Complex is a type where we have >1 conditional layers
  type <- character()
  .aliases <- character()
  if (!missing(confounders) | !missing(scores)){
    if (!(length(unlist(confounders)) == length(unlist(scores))))
      stop('Length mismatched!')

    if (is.character(confounders)) type <- 'binary' else type <- 'categorical'

    if (type == 'categorical' & length(names(scores)) < length(confounders))
      stop('Score must be named in multi-level conditions')

    .aliases <-
      if (type == 'binary') confounders
    else unlist(lapply(seq_along(confounders),
                       function(i)
                         rep(names(confounders[i]), length(confounders[[i]]))))

    # if (!length(names(aliases))){
    #   names(aliases) <- aliases
    #   aliases <- .aliases
    # }
  }
  if (!missing(custom_cases)) type <- c(type, 'complex')
  if ('complex'%in%type){
    .aliases <- unique(c(.aliases, names(custom_cases)))
  }

  # browser()

  .aliases <- unique(.aliases)
  .aliases <-
    if (length(aliases) & length(names(aliases))) dplyr::recode(.aliases, !!!aliases)
  else dplyr::recode(.aliases, !!!structure(aliases, names = .aliases))

  # browser()

  #Construction condition tree from confounder and scores
  if (!missing(confounders)){
    conf <-
      if ('binary' %in% type) structure(rep(TRUE, length(confounders)), names = confounders)
    else confounders

    if ('binary' %in% type) scores <- structure(scores, names = confounders)
    simple_cases <- .tree_construct(conf, scores)
    names(simple_cases$name) <- simple_cases$name
  } else simple_cases <- NULL

  if (!missing(custom_cases)){
    # browser()
    custom_cases_var <-
      lapply(custom_cases, function(custom_case) {
        out <- unique(unlist(lapply(custom_case, all.vars)))
        out[!out %in% c('.', '.id')]
      })
    names(custom_cases_var) <- names(custom_cases)
    custom_cases <- list(name = names(custom_cases),
                         var = custom_cases_var,
                         fml = custom_cases)
    # cases <- c(simple_cases, custom_cases)
  } else custom_cases <- NULL

  cases <- list(name = c(simple_cases$name, custom_cases$name),
                fml = c(simple_cases$fml, custom_cases$fml),
                var = c(simple_cases$name, custom_cases$var))
  # browser()

  score_table <- structure(cases$fml,
                           names = cases$name,
                           score_names = paste(cases$name, 'score', sep = '_'),
                           vars = cases$var,
                           aliases = .aliases)


  score_object <- structure(
    function(data, id = names(data)[1], which = names(score_table),...){
      if (missing(data)) get(deparse(sys.call()[[1]]))
      else purrr::partial(.calc, score_table = score_table)(data, id, which, ...)
    },
    class = c('ScoreTable', 'function'),
    fml = cases$fml,
    name = cases$name,
    score_name = paste(cases$name, 'score', sep = '_'),
    alias = .aliases,
    score_table = score_table
  )
  return(score_object)
}


.tree_construct <- function(conf, scores){
  # browser()
  conf_name <- names(conf)
  conf_fml <-
    lapply(conf_name,
           function(.conf_name){
             .conf <- unlist(conf[names(conf) == .conf_name])
             .score <- unlist(scores[names(scores) == .conf_name])
             c(
               unlist(lapply(seq_along(.conf),
                             function(i){
                               as.formula(paste(.conf_name, '==', .conf[i], '~', .score[i]))
                             })),
               as.formula(paste('!is.na(', .conf_name, ') ~ 0'))
             )
           })
  names(conf_fml) <- conf_name
  return(list(name = conf_name, fml = conf_fml))
}

#' A method to print out ScoreTable object
#' @description A method to print out ScoreTable object
#' @method print ScoreTable
#' @param x An object of class ScoreTable
#' @param pretty A logical value. Default = TRUE will print out the pretty version of the table.
#' @seealso \link{as.data.frame.ScoreTable}, \link{huxtable.ScoreTable}, \link{flextable.ScoreTable}
#' @export
print.ScoreTable <- function(x, pretty = TRUE,...){
  print(as.data.frame(x, pretty = pretty))
}

#' A method to coerce ScoreTable object to analysable data frame
#' @description A method to coerce ScoreTable object to analysable data frame
#' @method as.data.frame ScoreTable
#' @param x An object of class ScoreTable
#' @param pretty A logical value. Default = FALSE will create a analysable version of the table.
#' @param ... Additional parameters passed to data.frame()
#' @seealso \link{print.ScoreTable}, \link{huxtable.ScoreTable}, \link{flextable.ScoreTable}
#' @export
as.data.frame.ScoreTable <- function(x, pretty = FALSE, ...){
  score_table <- attr(x, 'score_table')
  aliases <- attr(x, 'alias')
  aliases.expand <- unlist(lapply(attr(x, 'name'),
                                  function(name){
                                    if (pretty)
                                      c(aliases[attr(x, 'name') == name], rep('', length(score_table[[name]]) -1))
                                    else
                                      c(rep(aliases[attr(x, 'name') == name], length(score_table[[name]])))
                                  }))
  fml <- attr(x, 'fml')

  # browser()
  condition_score <- lapply(lapply(fml, function(.fml) as.character(.fml)), strsplit, '\\s*~\\s*', perl = TRUE)
  condition <- unlist(lapply(condition_score,
                             function(.condition_score) sapply(.condition_score,
                                                               function(.c_s) .c_s[1])))
  score <- unlist(lapply(condition_score,
                         function(.condition_score) sapply(.condition_score,
                                                           function(.c_s) .c_s[2])))

  # browser()
  dt <- data.frame(Variable = aliases.expand, Condition = condition, Score = score, ...)
  dt
}

#' @export
flextable <- function(x,...){
  UseMethod('flextable')
}

#' A method to convert score table to flextable
#' @description A method to convert score table to flextable
#' @method flextable ScoreTable
#' @param x An object of class ScoreTable
#' @param ... Additional params passed to flextable::flextable.
#' @seealso \link{print.ScoreTable}, \link{huxtable.ScoreTable}, \link{as.data.frame.ScoreTable},
#'  \link[flextable]{flextable}
#' @export
flextable.ScoreTable <- function(x,...){
  flextable::flextable(as.data.frame(x, pretty = TRUE),...)
}

#' @export
huxtable <- function(x,...){
  UseMethod('huxtable')
}

#' A method to convert score table to huxtable
#' @description A method to convert score table to huxtable
#' @method huxtable ScoreTable
#' @param x An object of class ScoreTable
#' @param ... Additional params passed to huxtable::hux.
#' @seealso \link{print.ScoreTable}, \link{huxtable.ScoreTable}, \link{as.data.frame.ScoreTable},
#' \link[huxtable]{huxtable}
#' @export
huxtable.ScoreTable <- function(x,...){
  hux <- huxtable::huxtable(as.data.frame(x, pretty = TRUE))
  C306::ht_theme_markdown(hux, header_rows = 1)
}
