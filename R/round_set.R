#' Change the Digital for Double
#'
#' @param x a double number
#' @param value digital number
#'
#' @return double number
#' @export
#'
#' @examples
#' x = 3.123
#' #usual method
#' x = round(x, 3)
#' #now
#' round(x) = 3
`round<-` <- function(x,value){
    round(x,value)
}
