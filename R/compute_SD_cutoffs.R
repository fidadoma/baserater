#' Computes SD limits for given SD
#'
#' @param df data frame with variables
#' @param v vector of variable names, for which we compute the SD cutoffs
#' @param sdi SD scaling factor
#' @param grouping_var1 first variable to create subgroups
#' @param grouping_var2 second variable to create subgroups
#' @param lower_lim_suffix suffix for the variables that contain lower limits
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
#' compute_SD_cutoffs_limits(controls2, v, sett)
compute_SD_cutoffs_limits <- function(df, v, sett, by_gr1=F, by_gr2=F) {
  lower_lim_suffix <- sett$lower_lim_suffix
  grouping_var1    <- sett$grouping_var1
  grouping_var2    <- sett$grouping_var2
  if(by_gr1 & by_gr2) {
    df %>%
      group_by(sdi, !!grouping_var1, !!grouping_var2) %>%
      summarize_at(v, list( ~lower_lim(., sd_lim = sdi[1], na.rm = T))) %>%
      rename_at(v,list(~paste0(.,lower_lim_suffix)))
  } else if(by_gr1 & !by_gr2) {
    df %>%
      group_by(sdi, !!grouping_var1) %>%
      summarize_at(v, list( ~lower_lim(., sd_lim = sdi[1], na.rm = T))) %>%
      rename_at(v,list(~paste0(.,lower_lim_suffix)))
  } else if(!by_gr1 & by_gr2) {
    df %>%
      group_by(sdi, !!grouping_var2) %>%
      summarize_at(v, list( ~lower_lim(., sd_lim = sdi[1], na.rm = T))) %>%
      rename_at(v,list(~paste0(.,lower_lim_suffix)))
  } else { #(!by_gr1 & !by_gr2)
    df %>%
      group_by(sdi) %>%
      summarize_at(v, list( ~lower_lim(., sd_lim = sdi[1], na.rm = T))) %>%
      rename_at(v,list(~paste0(.,lower_lim_suffix)))
  }
}


