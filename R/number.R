#' number
#'
#' @param x
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
no <- function(x){
  value_no <- x %>% filter(unit %in% c("NO","NOMT","Number of fish","no"))
  return(sum(value_no$value))
}


