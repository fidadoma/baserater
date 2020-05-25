#' Creates shared settings
#'
#' @param gr1 quoted name of first variable
#' @param gr2 quoted name of first variable
#'
#' @return
#' @export
#'
#' @examples
#' sett <- create_settings(quo(Age_categ),quo(Sex))
create_settings <- function(gr1,gr2) {
  list(
    lower_lim_suffix = "_br_lower_lim",
    is_lower_suffix  = "_br_is_lower",
    possprob_suffix_gr1 = paste0("_",rlang::as_label(gr1)),
    possprob_suffix_gr2 = paste0("_",rlang::as_label(gr2)),
    possprob_suffix_gr1gr2 = paste0("_",rlang::as_label(gr1),rlang::as_label(gr2)),
    n_lower_col = "n_lower",
    grouping_var1 = gr1,
    grouping_var2 = gr2,
    sd_col = "sdi"
  )
}

