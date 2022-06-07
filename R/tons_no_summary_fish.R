#' Summary_fishe
#'
#' @param x
#'
#' @return
#' @import dplyr
#'
#' @examples
#'
tons <- function(x){
  value_tons <- x %>% filter(unit %in% c("MT","MTNO","Tons"))
  return(sum(value_tons$value))
}


