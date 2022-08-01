#' Title
#'
#' @param dimension
#' @param first
#' @param second
#' @param topn
#' @param titre_1
#' @param titre_2
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
#'
pie_chart_2 = function(dimension, first, second, topn = 5, titre_1 = "first", titre_2 = "second" ) {

  if(any(first$unit == "MTNO")) first[first$unit == "MTNO", ]$unit <- "MT"
  if(any(first$unit == "NOMT")) first[first$unit == "NOMT", ]$unit <- "NO"
  if(any(second$unit == "MTNO")) second[second$unit == "MTNO", ]$unit <- "MT"
  if(any(second$unit == "NOMT")) second[second$unit == "NOMT", ]$unit <- "NO"
  first[is.na(first)] <- "NA"
  second[is.na(second)] <- "NA"

  r <- deparse(substitute(dimension))

  colnames <- dplyr::enquo(dimension)

  name1 <- dplyr::enquo(titre_1)
  name2 <- dplyr::enquo(titre_2)

  provisoire_i <-na.omit(first) %>%  dplyr::group_by(!!colnames, unit
  )  %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% dplyr::group_by(unit) %>%
    dplyr::arrange(desc(value)) %>%   dplyr::mutate(id = row_number())%>%
    dplyr::mutate(class = as.factor(ifelse(id<topn  ,!!colnames,"Others"))) %>%
    dplyr::group_by(class, unit
    ) %>%  dplyr::summarise(value = sum(value, na.rm = TRUE))%>%dplyr::ungroup()%>%
    dplyr::select( value, class,unit) %>%   dplyr::group_by(unit) %>%
    dplyr::mutate(pourcentage = prop.table(value)*100)%>%
    dplyr::mutate(labels = paste0(pourcentage," ",  " % "))%>% dplyr::arrange(desc(class))%>%
    dplyr::mutate(ypos_ligne = cumsum(pourcentage)- 0.5*pourcentage ) %>%
    dplyr::distinct() %>% dplyr::filter(!is.na(class))




  provisoire_t <- na.omit(second) %>%  dplyr::group_by(!!colnames, unit)%>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% dplyr::group_by(unit) %>%
    dplyr::arrange(desc(value)) %>%   dplyr::mutate(id = row_number())%>%
    dplyr::mutate(class = as.factor(ifelse(id<topn,!!colnames,"Others"))) %>%
    dplyr::group_by(class, unit
    ) %>%  dplyr::summarise(value = sum(value, na.rm = TRUE))%>%dplyr::ungroup()%>%
    dplyr::select( value, class,unit
    ) %>%   dplyr::group_by(unit) %>%
    dplyr::mutate(pourcentage = prop.table(value)*100)%>%
    dplyr::mutate(labels = paste0(pourcentage," ",  " % "))%>% dplyr::arrange(desc(class))%>%
    dplyr::mutate(ypos_ligne = cumsum(pourcentage)- 0.5*pourcentage ) %>%
    dplyr::distinct()%>% dplyr::filter(!is.na(class))

  set.seed(2) # For reproducibility of random color vector
  # print(c(provisoire_i$class, provisoire_t$class))
  # print(unique(unlist(as.character(c(provisoire_i$class, provisoire_t$class)))))
  number <- length(unique(unlist(as.character(c(provisoire_i$class, provisoire_t$class)))))
  # print(number)
  pal <- brewer.pal(number,"Paired")
  #   display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
  # colorblindFriendly=TRUE)
  # print(number)

  # myColors <- ColorRampPalette(brewer.pal(120,"Spectral"))
  # names(myColors) <- levels(dat$grp)
  # set.seed(2)
  # print(pal)
  pal = setNames(pal, unique(unlist(as.character(c(provisoire_i$class, provisoire_t$class)))))
  # print(pal)


  ggplot_i <<- ggplot(provisoire_i%>% dplyr::filter(!is.na(class))) +
    aes(
      x = "",
      fill = class,
      group = class,
      weight = pourcentage
    ) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_minimal()+ coord_polar("y", start=0)+ geom_text(first = (provisoire_i %>% dplyr::filter(!is.na(class))%>%dplyr::mutate_if(is.numeric, round)), size = 3,
                                                          aes( x = 1 ,y = ypos_ligne/100, label = paste0(round(pourcentage),"%")), color = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = "", y="")+ scale_fill_manual(values=pal)+ guides(fill=guide_legend(title=toupper(r)))+facet_wrap("unit")+
    scale_fill_discrete(na.translate = F)
  # print("ggplot_i")
  legend <- cowplot::get_legend(ggplot_i+
                                  scale_fill_discrete(na.translate = F))

  ggplot_t <<- ggplot(provisoire_t%>% dplyr::filter(!is.na(class))) +
    aes(
      x = "",
      fill = class,
      group = class,
      weight = pourcentage
    ) +
    geom_bar(position = "fill") +
    scale_fill_hue(direction = 1) +
    scale_color_hue(direction = 1) +
    theme_minimal()+ coord_polar("y", start=0)+ geom_text(first = (provisoire_t%>% dplyr::filter(!is.na(class)) %>%dplyr::mutate_if(is.numeric, round)), size = 3,
                                                          aes( x = 1, y = ypos_ligne/100, label = paste0(round(pourcentage),"%")), color = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = "", y="")+ scale_fill_manual(values=pal)+ theme(legend.position = "none")+facet_wrap("unit")+
    scale_fill_discrete(na.translate = F)

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


  graph <<- plot_grid(ggplot_i+ theme(legend.position = "none"), ggplot_t, nrow = 2,labels = c( gsub('"','',gsub('~"','',deparse(substitute(name1)))),gsub('"','',gsub('~"','',deparse(substitute(name2))))),
                      label_size = 10, vjust = 1.3, label_x = c(0,0), label_y = 1.025, axis = "l", align = "v")

  plot_grid(title, nrow=2,plot_grid(graph,legend,ncol = 2),
            # rel_heights values control vertical title margins
            rel_heights = c(0.1, 1))+
    theme(plot.background = element_rect(color = "black"))


}
#test for git
