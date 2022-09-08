#'  resume_knit_child
#'
#' @param dimension dimension for groupping
#' @param first first table
#' @param second second table
#'
#' @import knitr
#'
#'
#' @return Knit_child of pie and table
#' @export
#'
#' @examples
#'
#'

resume_knit_child = function(dimension, first, second = NULL){
  if (deparse(substitute(dimension)) == "X[[i]]"){ #for sapply function bug
    r <- dimension
  }else { r <- deparse(substitute(dimension))}
  knitr::knit_child(text = c(
    "```{r results='asis', fig.cap = paste0('Distribution in value for the dimension : ',dimension)}",
    "studycatchesird::pie_chart_2(dimension, first, second, title_yes_no = FALSE)",
    "```",
    "",
    "",
    "",
    "```{r results='asis'}",
    "df <- data.frame(matrix(ncol = 3, nrow = 0))",
    "colnames(df) <- c('var1', 'var2', 'var3')",
    "qflextable2(df, captionn = 'Test333')",
    "```"



  ), envir = global_env())
}
