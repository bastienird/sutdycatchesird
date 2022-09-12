#'  pie_chart_2
#'
#' @param dimension dimension for grouppiing
#' @param first first table
#' @param second second table
#' @param topn number before groupping in Others
#' @param titre_1 name of first table
#' @param titre_2 name of second table
#'
#' @import cowplot
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import RColorBrewer
#' @import rlang
#'
#' @return Several pie maps
#' @export
#'
#' @examples
#'
#'
pie_chart_2 =function (dimension, first, second = NULL, topn = 4, titre_1 = "first",
                       titre_2 = "second", title_yes_no = TRUE, dataframe = FALSE)
{

  if (any(first$unit == "MTNO"))
    first[first$unit == "MTNO", ]$unit <- "MT"
  if (any(first$unit == "NOMT"))
    first[first$unit == "NOMT", ]$unit <- "NO"
  if (!is.null(second)) {
    if (any(second$unit == "MTNO"))
      second[second$unit == "MTNO", ]$unit <- "MT"
    if (any(second$unit == "NOMT"))
      second[second$unit == "NOMT", ]$unit <- "NO"
    second[is.na(second)] <- "NA"
  }
  first[is.na(first)] <- "NA"
  if (deparse(substitute(dimension)) == "X[[i]]"){ #for sapply function bug
    r <- dimension
  }else { r <- deparse(substitute(dimension))}

  if (r == "source_authority") {
    topn = 6
  }
  if(gsub("\"","",r) == r){
    dimension <- r
  }

  name1 <- dplyr::enquo(titre_1)
  name2 <- dplyr::enquo(titre_2)

  all_class_i <- first %>% dplyr::group_by(across(c(dimension,
                                                     "unit"))) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% filter(value != 0)


  provisoire_i <- na.omit(first) %>% dplyr::group_by(dplyr::across(c(dimension,
                                                                     "unit"))) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(unit) %>% dplyr::arrange(desc(value)) %>%
    dplyr::mutate(id = row_number()) %>% dplyr::mutate(class = as.factor(ifelse(id <
                                                                                  topn,!!rlang::sym(dimension), "Others"))) %>% dplyr::group_by(class,
                                                                                                                                                unit) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% dplyr::select(value, class, unit) %>%
    dplyr::group_by(unit) %>% dplyr::mutate(pourcentage = prop.table(value) *
                                              100) %>% dplyr::mutate(labels = paste0(pourcentage,
                                                                                     " ", " % ")) %>% dplyr::arrange(desc(class)) %>% dplyr::mutate(ypos_ligne = cumsum(pourcentage) -
                                                                                                                                                      0.5 * pourcentage) %>% dplyr::distinct() %>% dplyr::filter(!is.na(class))
  if (!is.null(second)) {
    all_class_t <- second %>% dplyr::group_by(across(c(dimension,
                                                       "unit"))) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% filter(value != 0)
    provisoire_t <- na.omit(second) %>% dplyr::group_by(across(c(dimension,
                                                                 "unit"))) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::group_by(unit) %>% dplyr::arrange(desc(value)) %>%
      dplyr::mutate(id = row_number()) %>% dplyr::mutate(class = as.factor(ifelse(id <
                                                                                    topn, !!rlang::sym(dimension), "Others"))) %>% dplyr::group_by(class,
                                                                                                                                                   unit) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::select(value, class,
                                         unit) %>% dplyr::group_by(unit) %>% dplyr::mutate(pourcentage = prop.table(value) *
                                                                                             100) %>% dplyr::mutate(labels = paste0(pourcentage,
                                                                                                                                    " ", " % ")) %>% dplyr::arrange(desc(class)) %>%
      dplyr::mutate(ypos_ligne = cumsum(pourcentage) -
                      0.5 * pourcentage) %>% dplyr::distinct() %>%
      dplyr::filter(!is.na(class))
  }
  if(!is.null(second)){
    disappearing_stratas <- anti_join(all_class_i %>% select(class), all_class_t%>% select(class))%>% distinct()
    appearing_stratas <- anti_join(all_class_t%>% select(class), all_class_i%>% select(class)) %>% distinct()
    number_disappearing_stratas <- nrow(disappearing_stratas)
    number_appearing_stratas <- nrow(appearing_stratas)
    summary_apparition <- ggdraw()+draw_label(paste0("Number of appearing stratas : ", number_appearing_stratas), size = 10)
    if(number_appearing_stratas!= 0)summary_apparition <- summary_apparition+draw_label(paste0(" \nThey are ", paste((appearing_stratas %>% select(class) %>% distinct())$class, sep = ";")), size = 10)
    summary_apparition <- summary_apparition+draw_label(paste0(" \nNumber of disappearing stratas : ", number_disappearing_stratas), size = 10)
    if(number_disappearing_stratas!= 0)summary_apparition <- summary_apparition+draw_label(paste0(" \nThey are ", paste((disappearing_stratas %>% select(class) %>% distinct())$class, sep = ";")), size = 10)
  }

  set.seed(2)
  if (!(is.null(second))) {
    number <- length(unique(unlist(as.character(c(provisoire_i$class,
                                                  provisoire_t$class)))))
  }
  else {
    number <- length(unique(unlist(as.character(c(provisoire_i$class)))))
  }
  pal <- brewer.pal(number, "Paired")
  if (!(is.null(second))) {
    pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class,
                                                     provisoire_t$class)))))
  }
  else {
    pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class)))))
  }

  ggplot_i <- ggplot(provisoire_i %>% dplyr::filter(!is.na(class))) +
    aes(x = "", fill = class, group = class, weight = pourcentage) +
    geom_bar(position = "fill") + scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) + theme_minimal() + coord_polar("y",
                                                                   start = 0) + geom_text(first = (provisoire_i %>% dplyr::filter(!is.na(class)) %>%
                                                                                                     dplyr::mutate_if(is.numeric, round)), size = 3, aes(x = 1,
                                                                                                                                                         y = ypos_ligne/100, label = paste0(round(pourcentage),
                                                                                                                                                                                            "%")), color = "black") + theme(axis.ticks.x = element_blank(),
                                                                                                                                                                                                                            axis.text.x = element_blank()) + labs(x = "", y = "") +
    scale_fill_manual(values = pal) + guides(fill = guide_legend(title = toupper(r))) +
    facet_wrap("unit") + scale_fill_discrete(na.translate = F)
  if (!is.null(second)) {
    to_get_legend <- ggplot(rbind(provisoire_i %>% dplyr::filter(!is.na(class)),
                                  provisoire_t %>% dplyr::filter(!is.na(class)))) +
      aes(x = "", fill = class, group = class, weight = pourcentage) +
      geom_bar(position = "fill")+ guides(fill = guide_legend(title = toupper(r)))
    legend <- cowplot::get_legend(to_get_legend + scale_fill_discrete(na.translate = F))
    ggplot_t <<- ggplot(provisoire_t %>% dplyr::filter(!is.na(class))) +
      aes(x = "", fill = class, group = class, weight = pourcentage) +
      geom_bar(position = "fill") + scale_fill_hue(direction = 1) +
      scale_color_hue(direction = 1) + theme_minimal() +
      coord_polar("y", start = 0) + geom_text(first = (provisoire_t %>%
                                                         dplyr::filter(!is.na(class)) %>% dplyr::mutate_if(is.numeric,
                                                                                                           round)), size = 3, aes(x = 1, y = ypos_ligne/100,
                                                                                                                                  label = paste0(round(pourcentage), "%")), color = "black") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
      labs(x = "", y = "") + scale_fill_manual(values = pal) +
      theme(legend.position = "none") + facet_wrap("unit") +
      scale_fill_discrete(na.translate = F)
  }
  else {
    legend <- cowplot::get_legend(ggplot_i + scale_fill_discrete(na.translate = F))
  }
  title <- ggdraw() + draw_label(paste0("Distribution in value for the dimension : ",
                                        r), fontface = "bold", x = 0, hjust = 0) + theme(plot.margin = margin(0,
                                                                                                              0, 0, 7))
  if (!is.null(second)) {
    graph <- plot_grid(ggplot_i + theme(legend.position = "none"),
                       ggplot_t, nrow = 2, labels = c(gsub("\"", "", gsub("~\"",
                                                                          "", deparse(substitute(name1)))), gsub("\"",
                                                                                                                 "", gsub("~\"", "", deparse(substitute(name2))))),
                       label_size = 10, vjust = 1.3, label_x = c(0, 0),
                       label_y = 1.025, axis = "l", align = "v")
    ploting_map <- plot_grid(title, nrow = 2, plot_grid(graph,
                                                        legend, ncol = 2), rel_heights = c(0.1, 1)) + theme(plot.background = element_rect(color = "black"))
    if (sum(!(round(provisoire_i$pourcentage) == round(provisoire_t$pourcentage))) ==
        0) {
      title <- ggdraw() + draw_label(paste0("Distribution in value for the dimension : ",
                                            r, "\n(same distribution to the nearest rounding for both datasets : ",
                                            gsub("\"", "", gsub("~\"",
                                                                "", deparse(substitute(name1))))," and ", gsub("\"",
                                                                                                               "", gsub("~\"", "", deparse(substitute(name2)))),")"),
                                     fontface = "bold", x = 0, hjust = 0, vjust = 0.5, size = 13) + theme(plot.margin = margin(0,
                                                                                                                               0, 0, 7))
      graph <- ggplot_i+ theme(legend.position = "none")
    }
  }
  else {
    graph <- plot_grid(ggplot_i + theme(legend.position = "none"),
                       nrow = 1, labels = c(gsub("\"", "", gsub("~\"",
                                                                "", deparse(substitute(name1))))), label_size = 10,
                       vjust = 1.3, label_x = c(0, 0), label_y = 0.8, axis = "l",
                       align = "v")

  }
  if(!title_yes_no){
    if (exists("provisoire_t")) if(sum(!(round(provisoire_i$pourcentage) == round(provisoire_t$pourcentage))) ==
        0) {      title <- ggdraw() + draw_label(paste0("(same distribution to the nearest rounding for both datasets :\n",
                                                        gsub("\"", "", gsub("~\"",
                                                                            "", deparse(substitute(name1))))," and ", gsub("\"",
                                                                                                                           "", gsub("~\"", "", deparse(substitute(name2)))),")"),
                                                 fontface = "bold", x = 0, hjust = 0, vjust = 0.5, size = 13) + theme(plot.margin = margin(0,
                                                                                                                                           0, 0, 7))}
    else {
      title <- ggdraw() + draw_label(" \n ")
    }
  }

  ploting_map <- plot_grid(title, nrow = 2, plot_grid(graph,
                                                      legend, ncol = 2), rel_heights = c(0.1, 1)) + theme(plot.background = element_rect(color = "black"))
  if(exists("summary_apparition") & dataframe){
    df <- data.frame(' '= c("Stratas appearing","Stratas disappearing"),
                     'Number' = c(number_appearing_stratas,number_disappearing_stratas ),
  'Detail' = c(toString(paste((appearing_stratas %>% select(class) %>% mutate(class = gsub(" ", "",class) %>% distinct()))$class, sep = ";")),
     toString(paste((disappearing_stratas %>% select(class)%>% mutate(class = gsub(" ", "",class) %>% distinct()))$class, sep = ";"))),check.names = FALSE,fix.empty.names = FALSE)
    if(number_disappearing_stratas == 0 & number_appearing_stratas ==0){df <- df %>% select(-Detail)}
    list_df_plot <- list(plot = ploting_map, df =df)
  return(list_df_plot)
} else{return(ploting_map)}
}



