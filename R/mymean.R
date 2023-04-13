#' Test Driven Development
#'
#' A small example to see how far we come with implementing test driven development
#' in a very controlled situation with a definite and simple solution we can check.
#' @param x vector of numbers
#' @param na.rm boolean, should missing values in vector x be ignored?
#' @return average of x
#' @export
#' @importFrom stats na.omit
#' @importFrom utils head
mymean <- function(x, na.rm=T) {
  if (!is.numeric(x)) {
    warning(sprintf("Vector x is not numeric %s", paste(head(x), sep=",", collapse=",")))
    return(NA)
  }
  if (na.rm) x <- na.omit(x)
  sum(x)/length(x)
}
