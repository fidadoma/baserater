#' Compute baserate cutoff for division into possible and probable categories
#' Currently expects 0-2 variables that breaks the data
#' This function should be run after computing number of low scores.
#' The column name with lower scores is expected to be named n_lower
#'
#' @param df tibble with data
#' @param v1 first variable, that divide the data. Can be null, when we are only interested in overall data
#' @param v2 second variable, that divide the data. Can be null, when we are only interested in one variable
#' @param possible - quantile, which is threshold for classification into possible
#' @param probable - quantile, which is threshold for classification into probable
#' @param round_dg to how many decimal places should we round
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(baserater)
#' data(baserate_UDS)
#' v <- controls %>% select(MMSE_total:BNT) %>% colnames()
#' sett <- create_settings(quo(gender),quo(edu_cat))
#' sdi <- c(2,1.5,1)
#' controls2 <- multiply_df(controls, sdi)
#' compute_SD_cutoffs_limits(controls2,v, sett, by_gr1 = T, by_gr2 = T) %>%
#'   add_SD_cutoffs(controls2, .,sett) %>%
#'   test_if_lower(v,sett) %>%
#'   compute_nlower(v,sett) %>%
#'   compute_probposs_baserate_cutoffs(sett$grouping_var1, sett$grouping_var2, possible = .8, probable = .9)
#'   
compute_probposs_baserate_cutoffs <- function( df, v1 = NULL, v2 = NULL, possible = .8, probable = .9, round_dg = 3) {
  if(!(possible>0&possible<1)) {
    stop("parameter possible should be larger than 0 and smaller than 1")
  }

  if(!(probable>0&probable<1)) {
    stop("parameter probable should be larger than 0 and smaller than 1")
  }

  if(!is.null(v2) & !is.null(v1)) {
    tmp <- df %>%
      group_by(sdi,!! v1, !! v2) %>%
      summarize(n = n(),
                q_possible = quantile(n_lower,probs = c(possible)),
                q_possible_perc = 100*(1 - ecdf(n_lower)(q_possible)) %>% round(round_dg),
                q_probable = quantile(n_lower,probs = c(probable)),
                q_probable_perc = 100*(1 - ecdf(n_lower)(q_probable))%>% round(round_dg))
  } else if(!is.null(v1)) {
    tmp <- df %>%
      group_by(sdi,!!v1) %>%
      summarize(n = n(),
                q_possible = quantile(n_lower,probs = c(possible)),
                q_possible_perc = 100*(1 - ecdf(n_lower)(q_possible)) %>% round(round_dg),
                q_probable = quantile(n_lower,probs = c(probable)),
                q_probable_perc = 100*(1 - ecdf(n_lower)(q_probable))%>% round(round_dg))
  } else {
    tmp <- df %>%
      summarize(n = n(),
                q_possible = quantile(n_lower,probs = c(possible)),
                q_possible_perc = 100*(1 - ecdf(n_lower)(q_possible)) %>% round(round_dg),
                q_probable = quantile(n_lower,probs = c(probable)),
                q_probable_perc = 100*(1 - ecdf(n_lower)(q_probable))%>% round(round_dg))
  }

  return(tmp)
}
