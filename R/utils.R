#' Computes lower limit for subscale
#'
#' Lower limit is compute by subtracting specified multiplier of SD from the mean
#'
#' @param x vector of values
#' @param sd_lim sd multiplier. Default value is 1.5
#' @param ... additional parameters supplied to mean and sd functions. This is good way how to supply na.rm=T parameter
#'
#' @return
#' @export
#'
#' @examples
#' x <- runif(500)
#' lower_lim(x)
#' lower_lim(x, sd_lim = 2)
#' lower_lim(x, sd_lim = 2, na.rm = T)
lower_lim <- function(x, sd_lim = 1.5, ...) {
  return(mean(x, ...) - sd_lim * sd(x, ...))
}

#' For given variables, creates variable names for lower limits
#'
#' @param v vector of variables
#' @param uniq_suffix unique suffix added to variables
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(baserater)
#' 
#' data(baserate_UDS)
#' 
#' sett <- create_settings(quo(gender),quo(edu_cat))
#' 
#' v <- controls %>% select(MMSE_total:BNT) %>% colnames()
#' v_lowerlim <- create_lower_lim_vars(v, sett)
create_lower_lim_vars <- function(v, sett) {
  uniq_suffix <- sett$is_lower_suffix
  v %>% paste0(uniq_suffix)
}



#' Multiples data frame for given SD. The column name will be named as sdi
#'
#' @param df data frame to multiply
#' @param sdi vector of SD scaling factors
#'
#' @return
#' @export
#'
#' @examples
#' data(iris)
#' multiply_df(iris, sdi = c(1,1.5))
multiply_df <- function(df, sdi) {

  df_out <- purrr::map_df(seq_len(length(sdi)), ~df) %>%
    dplyr::mutate(sdi = rep(sdi,each = nrow(df)))
  df_out
}

#' Tests, whether each of the variables is lower than appropriate lower limit
#'
#' This function is used for computation of number of low scores. First, limits should be computed based on control sample. Second, each score is compared with the limit to measure, whether subject scored below limit.
#'
#' @param df data frame that should contain all variables "v" and all variable "v+lower_lim_sufix"
#' @param sett list of settings
#' @param v vector of variable names used for computation of low scores
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(baserater)
#' 
#' data(baserate_UDS)
#' 
#' sett <- create_settings(quo(gender),quo(edu_cat))
#' 
#' v <- controls %>% select(MMSE_total:BNT) %>% colnames()
#' sdi <- c(1,1,5)
#' controls2 <- multiply_df(controls, sdi)
#' 
#' compute_SD_cutoffs_limits(controls2,v, sett, by_gr1 = T, by_gr2 = T) %>%
#' add_SD_cutoffs(controls2, .,sett) %>%
#'   test_if_lower(v,sett)
test_if_lower <- function(df, v, sett) {
  lower_lim_suffix <- sett$lower_lim_suffix
  output_suffix    <- sett$is_lower_suffix
  varnames <- df %>% dplyr::select_at(v) %>% colnames()
  varnames_lower <- paste0(varnames, lower_lim_suffix)
  varnames_is_lower <- paste0(varnames, output_suffix)

  for (i in 1:length(varnames)) {
    is_lower_name <- varnames_is_lower[i]
    df[[is_lower_name]] <- df[[varnames[i]]] < df[[varnames_lower[i]]]
  }
  df

}

#' Adds SD cutoffs to the data
#'
#' @param df original data
#' @param SD_cutoffs SD cutoffs for set of variables
#' @param sett list of settings
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(baserater)
#' 
#' data(baserate_UDS)
#' 
#' sett <- create_settings(quo(gender),quo(edu_cat))
#' 
#' v <- controls %>% select(MMSE_total:BNT) %>% colnames()
#' sdi <- c(1,1,5)
#' controls2 <- multiply_df(controls, sdi)
#' 
#' compute_SD_cutoffs_limits(controls2,v, sett, by_gr1 = T, by_gr2 = T) %>%
#' add_SD_cutoffs(controls2, .,sett) 
add_SD_cutoffs <- function(df, SD_cutoffs,sett) {
  join_by <- intersect(c(as_label(sett$grouping_var1),as_label(sett$grouping_var2),sett$sd_col), colnames(SD_cutoffs))

  df_out <- df %>% dplyr::left_join(SD_cutoffs, by = join_by)
  stopifnot(nrow(df_out)==nrow(df))
  stopifnot(ncol(df_out)==ncol(df)+ncol(SD_cutoffs)-length(join_by))
  df_out
}

#' Computes number of low scores
#'
#' @param df data frame
#' @param v set of variables
#' @param sett list of settings
#'
#' @return
#' @export
#'
#' @examples
#' #' library(tidyverse)
#' library(baserater)
#' 
#' data(baserate_UDS)
#' 
#' sett <- create_settings(quo(gender),quo(edu_cat))
#' 
#' v <- controls %>% select(MMSE_total:BNT) %>% colnames()
#' sdi <- c(1,1,5)
#' controls2 <- multiply_df(controls, sdi)
#' 
#' compute_SD_cutoffs_limits(controls2,v, sett, by_gr1 = T, by_gr2 = T) %>%
#' add_SD_cutoffs(controls2, .,sett) %>%
#' compute_nlower(v,sett) %>%
compute_nlower <- function(df, v, sett) {
  n_lower_col <- sett$n_lower_col
  limvars <- create_lower_lim_vars(v,sett)

  df %>%
    mutate(!!n_lower_col := df %>% dplyr::select_at(limvars) %>% rowSums(na.rm = T))
}
