# Below are functions that calculation the score table
match_name <- function(data, expected_names){
  names(data) <- tolower(names(data))
  notfound <- c()
  found <- c()
  for (expected_name in expected_names){
    if (expected_name %in% names(data)) {
      found <- append(found, expected_name)
    } else {
      actual_name <- grep(expected_name, names(data), ignore.case = TRUE, value = TRUE)
      # browser()
      if (!length(actual_name)){
        notfound <- append(notfound, expected_name)
      } else {
        message('\nName-matched variables:')
        actual_name <- actual_name[[1]]
        found <- append(found, expected_name)
        cat(crayon::green(actual_name), '-->', crayon::green(expected_name), '\n')
        # data <- rename(data, {{expected_name}} := {{actual_name}})
        names(data)[names(data) == actual_name] <- expected_name
      }
    }
  }

  if (length(notfound)){
    leftbehind <- names(data)[!names(data) %in% c('.id', found)]
    if (length(notfound) != length(leftbehind)) stop('Unmatched number of columns')
    message('\nOrder-matched variables:')
    for (i in seq_along(notfound))
      cat(crayon::green(leftbehind[i]), '-->', crayon::green(notfound[i]), '\n')
    names(data)[names(data) %in% leftbehind] <- notfound
  }
  return(as.data.frame(data))
}

#' @importFrom magrittr %>%
.calc <- function(data, score_table, id = names(data)[1], which = names(score_table), ...){
  # Preparing the data output
  data_call <- deparse(substitute(data))
  data <- as.data.frame(data)

  # stopifnot(all(which %in% names(score_table)))
  #Rename id
  if (length(id))  names(data)[names(data) == id] <- '.id'
  else data <- cbind(.id=1:nrow(data), data)

  #Rename columns based on map
  data <- dplyr::rename(data, ...)

  #Rename columns based on match
  # data <- match_name(data, which[!which %in% derived])
  needed_vars <-
    unique(
      unlist(
        lapply(which,
               function(i){
                 attr(score_table, 'vars')[names(attr(score_table, 'vars')) == i]
               })
        )
    )
  # browser()
  data <- match_name(data, needed_vars)

  # Build a score table
  score <-
    sapply(which,
           function(conf){
             # browser()
             # assign(conf, data[[conf]])
             fml <- score_table[[conf]]
             conf_score <- paste0(conf, '_score')
             # dplyr::mutate(data, {{conf_score}}:= case_when(!!!.env_adapt(fml)))
             # env = {cat('outer');print(ls(envir = parent.frame(2)))
             #   NULL}
             data %>% dplyr::mutate(
               {{conf_score}} := {
                 env_fml <- .env_adapt(fml, environment())
                 do.call(dplyr::case_when, env_fml)
                 # case_when(!!!.env_adapt(fml, env))
               }) %>% `[[`(conf_score)
           })

  # browser()
  score <- cbind(data['.id'], as.data.frame(score) %>% dplyr::mutate(total_score = rowSums(.)))
  if (length(id)) score <- dplyr::rename(score, {{id}} := .id)

  class(score) <- c('score_tbl', 'tbl', 'data.frame')
  attr(score, '.id') <- id
  return(score)
}

.env_adapt <- function(fml, env = parent.frame()){
  lapply(fml, function(.fml){
    attr(.fml,'.Environment') <- env
    return(.fml)
  })
}

#' Summary a score_tbl object
#' @description A function to do summarisation on a calculated score table.
#' @method summary score_tbl
#' @param x An object of class ScoreTable
#' @param method
#' A function to summarise the table.
#' For example: max, mean, min.
#' The function must return unique value for each ID.
#' This will be passed as .funs in \link[dplyr]{summarise_all}
#' @param ... Addition params passed to method
#' @return A tibble
#' @export
summary.score_tbl <- function(x, method = NULL, ...){
  if (!length(method)) NextMethod('summary')
  else{
    stopifnot(ncol(x)>1)
    .id <- rlang::as_quosure(as.formula(paste('~', attr(x, '.id'))))
    if (length(.id)) x <- dplyr::group_by(x, {{.id}})
    x %>% dplyr::summarise_all(method, ...)
  }
}
