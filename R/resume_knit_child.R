#'  pie_chart_2
#'
#' @param dimension dimension for grouppiing
#' @param first first table
#' @param second second table
#' @param topn number before groupping in Others
#' @param titre_1 name of first table
#' @param titre_2 name of second table
#'

#' @import knitr

#'
#' @return Several pie maps
#' @export
#'
#' @examples pie_chart_2("fishinfleet, init)
#'

resume_knit_child <- function(dimension, first, second = NULL){
  knitr::knit_child(text = c(
    "```{r results='asis', fig.cap = paste0('Distribution in value for the dimension : ',dimension))}",
    "studycatchesird::pie_chart_2(test, first, second, title_yes_no = FALSE)",
    "```",
    "",
    "",
    "",
    "```{r results='asis'}",
    "df <- data.frame(matrix(ncol = 3, nrow = 0))",
    "colnames(df) <- c('var1', 'var2', 'var3')",
    "qflextable2(df, captionn = 'Test333')",
    "```"



  ))
}
