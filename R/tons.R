#' tons
#'
#' @param x
#'
#' @return
#' @import dplyr
#' @export
#'
#'
#' @examples
#'
#'
tons <- function(x){
  value_tons <- x %>% filter(unit %in% c("MT","MTNO","Tons","t"))
  return(sum(value_tons$value))
}


