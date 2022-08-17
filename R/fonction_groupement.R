#' Title
#'
#' @param x
#' @param init
#' @param final
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
fonction_groupement = function(x, init, final){
  x  <-   dplyr::enquo(x)
  groupement_1  <-   init %>% dplyr::group_by(!!x,unit) %>% dplyr::summarise(number_lines1 = 0,
                                                                             value_sum_1 = round(sum(value, na.rm=TRUE))) %>%dplyr::mutate(value_sum_1 = ifelse(is.na(value_sum_1), 0, value_sum_1))
  groupement_2  <-   final %>% dplyr::group_by(!!x,unit) %>% dplyr::summarise(number_lines2 = 0,
                                                                              value_sum_2 = round(sum(value, na.rm=TRUE))) %>% dplyr::mutate(value_sum_2 = ifelse(is.na(value_sum_2), 0, value_sum_2))

  fulljoin  <-   full_join(groupement_1, groupement_2)%>%dplyr::mutate(value_sum_2 = ifelse(is.na(value_sum_2), 0, value_sum_2)) %>%dplyr::mutate(value_sum_1 = ifelse(is.na(value_sum_1), 0, value_sum_1))%>%dplyr::mutate(loss = value_sum_1 - value_sum_2) %>%dplyr::mutate(`Loss / Gain` = ifelse(loss >= 0, "Loss", "Gain")) %>%
    dplyr::mutate(Loss_pourcent = 100*((value_sum_1 - value_sum_2)/value_sum_1))%>%dplyr::mutate(Dimension = colnames(groupement_1[1])) %>%
    dplyr::rename("Precision" = 1) %>%dplyr::mutate(Precision = as.character(Precision)) %>%dplyr::mutate(value_sum_2 = ifelse(is.na(value_sum_2), 0, value_sum_2))%>%dplyr::mutate(Loss_pourcent = ifelse(is.na(Loss_pourcent)|Loss_pourcent==-Inf, 100, Loss_pourcent)) %>%dplyr::mutate(loss_nb_ligne = number_lines1 - number_lines2)%>%
    dplyr::mutate(Loss_en_milliers_de_tonne = (value_sum_1 - value_sum_2)/ 10^6) %>%
    dplyr::rename(`Difference (in %)` = Loss_pourcent,`Difference in millions of tons/fish` = Loss_en_milliers_de_tonne,
                  `Difference in number of lines` = loss_nb_ligne) %>%dplyr::mutate_if(is.numeric, list(~replace_na(., 0))) %>% dplyr::mutate(`Loss / Gain` = case_when(is.na(`Loss / Gain`) ~ "Gain",round(value_sum_1) == round(value_sum_2)~"Egal", TRUE ~ `Loss / Gain`))
}
