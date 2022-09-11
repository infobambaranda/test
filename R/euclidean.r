#' Euclidean
#'
#' @param a A numeric scalar
#' @param b A numeric scalar
#'
#' @return Return the greatest common divisor of the parameters a and b
#' @export
#'
#' @examples euclidean(123612, 13892347912)
#' euclidean(13892347912,123612)
#' euclidean(100, 1000)
euclidean <- function(a,b){
  while (b != 0){
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}
