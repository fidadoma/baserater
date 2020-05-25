#' Applies baserate approach to classify group into possible/probable
#'
#' @param df dataframe with group to classify
#' @param v which variables should be used for classification
#' @param df_controls data frame with controls used for computation of baserate scores
#' @param sdi vector of SDs for which we will classify the group when computing SD cutoffs
#' @param grouping_var1 first quoted variable name, for which we break down the data
#' @param grouping_var2 second quoted variable name, for which we break down the data
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(baserater)
#' v <- c("Extraversion","Empathy.Agreeableness","Conscientiousness","Instability.Neuroticism","Openness_to_Experience.Intellect","Honesty.Humility",
#'       "Disinhibition", "Detachment", "Psychoticism","Negative_Affect","Antagonism")
#' df <- tibble() # here should be proper dataset, for example neuropsychology::personality
#' mutate_at(df, v, ~(-.))
#' sett <- create_settings(quo(Sex),quo(Meditation))
#' df_controls <- df %>% filter(Mood_Disorder == "Absence")
#' df_patients <- df %>% filter(Mood_Disorder == "Presence")
#' sdi <- c(2,1.5,1)
#' pat_classified <- classify_group(df_patients,v,df_controls,sdi,sett = sett)

classify_group <- function(df,v, df_controls,sdi = c(2,1.5,1), sett) {
  grouping_var1 <- sett$grouping_var1
  grouping_var2 <- sett$grouping_var2

  df_controls2 <- multiply_df(df_controls, sdi)

  df_cutoffs_gr1_gr2 <-
    compute_SD_cutoffs_limits(df_controls2,v, sett, by_gr1 = T, by_gr2 = T)

  # Compute possible probable cutoffs for each subgroup on the control sample
  # currently working for two variables

  cutoff_gr1_gr2_possprob <- df_cutoffs_gr1_gr2 %>%
    add_SD_cutoffs(df_controls2, .,sett) %>%
    test_if_lower(v,sett) %>%
    compute_nlower(v,sett) %>%
    compute_probposs_baserate_cutoffs(grouping_var1, grouping_var2, possible = .8, probable = .9) %>%
    dplyr::select(-n,-q_possible_perc,-q_probable_perc)

  cutoff_gr1_possprob <-
    compute_SD_cutoffs_limits(df_controls2,v, sett, by_gr1 = T) %>%
    add_SD_cutoffs(df_controls2, .,sett) %>%
    test_if_lower(v,sett)%>%
    compute_nlower(v,sett) %>%
    compute_probposs_baserate_cutoffs(grouping_var1, possible = .8, probable = .9) %>%
    dplyr::select(-n,-q_possible_perc,-q_probable_perc)

  cutoff_gr2_possprob <-
    compute_SD_cutoffs_limits(df_controls2,v, sett, by_gr2 = T) %>%
    add_SD_cutoffs(df_controls2, .,sett) %>%
    test_if_lower(v,sett) %>%
    compute_nlower(v,sett) %>%
    compute_probposs_baserate_cutoffs(grouping_var2, possible = .8, probable = .9) %>%
    dplyr::select(-n,-q_possible_perc,-q_probable_perc)






  # actual classification
  # we need cutoffs values from controls, not the cutoffs from the tested sample
  df <- multiply_df(df, sdi)

  df <- df %>%
    dplyr::left_join(df_cutoffs_gr1_gr2, by = c("sdi",as_label(grouping_var1), as_label(grouping_var2))) %>%
    test_if_lower(v,sett)

  df <-
    df %>%
    evaluate_possible_probable(v, cutoff_gr1_gr2_possprob, sett, possprob_suffix = sett$possprob_suffix_gr1gr2) %>%
    evaluate_possible_probable(v, cutoff_gr1_possprob, sett, possprob_suffix = sett$possprob_suffix_gr1) %>%
    evaluate_possible_probable(v, cutoff_gr2_possprob, sett, possprob_suffix = sett$possprob_suffix_gr2)
  df
}
