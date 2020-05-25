#' Evaluate data into possible and probable.
#' This function is not meant to be run outside the package
#'
#' @param df dataframe that should be evaluated into possible/probable
#' @param v which variables are used for evaluation
#' @param sd_cutoffs dataframe with cutoffs for each variable
#' @param out_col name of ouotput column
#' @param join_by named vector that is supplied into join to merge data from cutoffs and evaluated data frame
#' @param is_lower_suffix suffix for is_lower_column
#' @param possprob_suffix suffix for possible/probable
#'
#' @return
#' @export
#'
#' @examples
evaluate_possible_probable <- function(df, v, sd_cutoffs, sett, possprob_suffix) {
  join_by <- intersect(c(as_label(sett$grouping_var1),as_label(sett$grouping_var2),sett$sd_col),colnames(sd_cutoffs))
  out_col         <- sym(sett$n_lower_col)
  is_lower_suffix <- sett$is_lower_suffix
  varnames_is_lower <- paste0(v, is_lower_suffix)
  out_q_possible    <- paste0("q_possible",possprob_suffix)
  out_q_probable    <- paste0("q_probable",possprob_suffix)
  possible_var      <- paste0("possible",possprob_suffix)
  probable_var      <- paste0("probable",possprob_suffix)

  df %>%
    dplyr::mutate(!!out_col := (df %>% dplyr::select_at(varnames_is_lower) %>% rowSums(na.rm = T))) %>%
    dplyr::left_join(sd_cutoffs, by = join_by) %>%
    dplyr::mutate(!!possible_var := (!!out_col > q_possible),
           !!probable_var := (!!out_col > q_probable),
           !!out_q_possible := q_possible,
           !!out_q_probable := q_probable) %>%
    dplyr::select(-q_possible, -q_probable)

}
