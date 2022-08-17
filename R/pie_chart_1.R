#' Title
#'
#' @param x
#' @param init
#'
#' @import cowplot
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import RColorBrewer
#' @import rlang
#'
#' @return
#' @export
#'
#' @examples
pie_chart_1 = function(x, init= init) {
  r <- deparse(substitute(x))
  colnames <- enquo(x)
  provisoire_i <-init %>%  group_by(!!colnames )%>% summarise(value = sum(value, na.rm = TRUE))  %>% arrange(desc(value)) %>%   mutate(id = row_number())%>%
    mutate(class = as.factor(ifelse(id<4,!!colnames,"Others"))) %>%
    group_by(class) %>%  summarise(value = sum(value, na.rm = TRUE))%>%ungroup()%>%
    select(  value, class)  %>% mutate(pourcentage = prop.table(value)*100)%>%
    mutate(labels = paste0(pourcentage," ",  " % "))%>% arrange(desc(class))%>%
    mutate(ypos_ligne = cumsum(pourcentage)- 0.5*pourcentage ) %>%
    distinct()



  ggplot_i <- ggplot(provisoire_i) +
    aes(
      x = "",
      fill = class,
      colour = class,
      group = class,
      weight = pourcentage
    ) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_minimal()+ coord_polar("y", start=0)+ geom_text(data = (provisoire_i %>% mutate_if(is.numeric, round)), size = 3,
                                                          aes( x = 1 ,y = ypos_ligne/100, label = paste0(pourcentage,"%")), color = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = "", y="")



  title <- ggdraw() +
    draw_label(
      paste0("Distribution in value for the dimension : ",r),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
titre <- enquo(init)
  graph <- plot_grid(ggplot_i,labels = c(deparse(substitute(titre))),
                     label_size = 10)

  plot_grid(
    title, graph,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1))+
    theme(plot.background = element_rect(color = "black"))


}
#
