#' @name dyadic
#' @rdname dyadic
#' @title \code{dyadic} objects
#' @description Creates and print dyadic objects
#' @param d a vector of 0/1
#' @return \code{dyadic} checks and...
NULL

#' @rdname dyadic
#' @export
is.dyadic <- function(x) inherits(x, "dyadic")

#' @rdname dyadic
#' @export
#' @param x an object to coerce to a \code{dyadic}
as.dyadic <- function(x, ...) UseMethod("as.dyadic")

#' @rdname dyadic
#' @export
as.dyadic.dyadic <- function(x) x

#' @rdname dyadic
#' @export
as.dyadic.integer <- function(x){
  if(!all(x %in% c(0L, 1L))) stop("a dyadic number must be a sequence of 0's and 1's")
  class(x) <- c("dyadic", "integer")
  return(setNames(x, seq_along(x)-1))
}

#' @rdname dyadic
#' @export
as.dyadic.numeric <- function(x){
  if(!all(x %in% c(0, 1))) stop("a dyadic number must be a sequence of 0's and 1's")
  return(as.dyadic.integer(as.integer(x)))
}

#' @rdname dyadic
#' @export
print.dyadic <- function(x, ...){
  print(unclass(x))
}
