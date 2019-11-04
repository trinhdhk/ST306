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
#' This is based on \link[dplyr]{case_when}
#' @return
#' An object of class ScoreTable.
#' When called with no data, this will print out the structure of the ScoreTable.
#' When called with data passed, this will return a data frame of class score_tbl.
#' @seealso
#' \link{as.data.frame.ScoreTable}, \link{apache.ii}, \link{summary.score_tbl}
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
#' @seealso \link{print.ScoreTable}, \link{huxtable.ScoreTable}, \link{flextable.ScoreTable}
#' @export
as.data.frame.ScoreTable <- function(x, pretty = FALSE){
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
  dt <- data.frame(Variable = aliases.expand, Condition = condition, Score = score)
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
#' @param ... Additional function passed to flextable::flextable.
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
#' @param ... Additional function passed to huxtable::hux.
#' @seealso \link{print.ScoreTable}, \link{huxtable.ScoreTable}, \link{as.data.frame.ScoreTable},
#' \link[huxtable]{huxtable}
#' @export
huxtable.ScoreTable <- function(x,...){
  hux <- huxtable::huxtable(as.data.frame(x, pretty = TRUE))
  C306::ht_theme_markdown(hux, header_rows = 1)
}
