#' Title
#'
#' @param x
#' @param first
#' @param second
#'
#' @import cowplot
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import RColorBrewer
#'
#' @return
#' @export
#'
#' @examples
pie_chart_2 = function(dimension, unit = c("MT","MTNO","Tons","t"), first, second) {
  r <- deparse(substitute(dimension))
  colnames <- enquo(dimension)
  name1 <- enquo(first)
  name2 <- enquo(second)
  first <- first %>% filter(unit%in%unit)
  second <- second %>% filter(unit%in%unit)
  provisoire_i <-first %>%  group_by(!!colnames#, unit
  )  %>% summarise(value = sum(value, na.rm = TRUE)) %>% #group_by(unit) %>%
    arrange(desc(value)) %>%   mutate(id = row_number())%>%
    mutate(class = as.factor(ifelse(id<4,!!colnames,"Others"))) %>%
    group_by(class#, unit
    ) %>%  summarise(value = sum(value, na.rm = TRUE))%>%ungroup()%>%
    select( value, class,#unit
    ) %>%  # group_by(unit) %>%
    mutate(pourcentage = prop.table(value)*100)%>%
    mutate(labels = paste0(pourcentage," ",  " % "))%>% arrange(desc(class))%>%
    mutate(ypos_ligne = cumsum(pourcentage)- 0.5*pourcentage ) %>%
    distinct()




  provisoire_t <- second %>%  group_by(!!colnames#, unit
  )%>% summarise(value = sum(value, na.rm = TRUE)) %>%# group_by(unit) %>%
    arrange(desc(value)) %>%   mutate(id = row_number())%>%
    mutate(class = as.factor(ifelse(id<4,!!colnames,"Others"))) %>%
    group_by(class#, unit
    ) %>%  summarise(value = sum(value, na.rm = TRUE))%>%ungroup()%>%
    select( value, class,#unit
    ) %>%#   group_by(unit) %>%
    mutate(pourcentage = prop.table(value)*100)%>%
    mutate(labels = paste0(pourcentage," ",  " % "))%>% arrange(desc(class))%>%
    mutate(ypos_ligne = cumsum(pourcentage)- 0.5*pourcentage ) %>%
    distinct()

# -------------------------------------------------------------------------




  set.seed(2) # For reproducibility of random color vector
  number <- length(unique(unlist(as.character(c(provisoire_i$class, provisoire_t$class)))))
  pal <- brewer.pal(number,"Set1")

  # myColors <- ColorRampPalette(brewer.pal(120,"Spectral"))
  # names(myColors) <- levels(dat$grp)
  # set.seed(2)
  pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class, provisoire_t$class)))))

  ggplot_i <<- ggplot(provisoire_i) +
    aes(
      x = "",
      fill = class,
      group = class,
      weight = pourcentage
    ) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_minimal()+ coord_polar("y", start=0)+ geom_text(first = (provisoire_i %>% mutate_if(is.numeric, round)), size = 3,
                                                          aes( x = 1 ,y = ypos_ligne/100, label = paste0(round(pourcentage),"%")), color = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = "", y="")+ scale_fill_manual(values=pal)+ guides(fill=guide_legend(title=toupper(r)))
  legend <- cowplot::get_legend(ggplot_i)

  ggplot_t <<- ggplot(provisoire_t) +
    aes(
      x = "",
      fill = class,
      group = class,
      weight = pourcentage
    ) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_minimal()+ coord_polar("y", start=0)+ geom_text(first = (provisoire_t %>% mutate_if(is.numeric, round)), size = 3,
                                                          aes( x = 1, y = ypos_ligne/100, label = paste0(round(pourcentage),"%")), color = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = "", y="")+ scale_fill_manual(values=pal)+ theme(legend.position = "none")

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
  graph <<- plot_grid(ggplot_i+ theme(legend.position = "none"), ggplot_t,nrow = 2,labels = c( deparse(substitute(name1)),deparse(substitute(name2))),
                      label_size = 10)

  plot_grid(title, nrow=2,plot_grid(graph,legend,ncol = 2),
            # rel_heights values control vertical title margins
            rel_heights = c(0.1, 1))+
    theme(plot.background = element_rect(color = "black"))


}
