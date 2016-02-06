#' @name dyadic2num
#' @rdname dyadic2num
#' @title Conversion between \code{dyadic} and \code{numeric}
#' @description Convert \code{dyadic} to \code{numeric} and vice-versa
#' @param x a dyadic number or a number between 0 (included) and 1 (excluded)
#' @param nmax integer, the maximal length of the dyadic number
#' @return \code{dyadic2num} returns a number between 0 and 1, \code{num2dyadic} returns a dyadic number
NULL

#' @rdname dyadic2num
#' @export
#' @param x an object to coerce to a \code{dyadic}
dyadic2num <- function(x) UseMethod("dyadic2num")

#' @rdname dyadic2num
#' @export
dyadic2num.dyadic <- function(x){
  return(sum(x/2L^(seq_along(x))))
}

#' @rdname dyadic2num
#' @export
dyadic2num.integer <- function(x) dyadic2num.dyadic(as.dyadic.integer(x))

#' @rdname dyadic2num
#' @export
dyadic2num.numeric <- function(x) dyadic2num.dyadic(as.dyadic.numeric(x))


#' @rdname dyadic2num
#' @export
num2dyadic <- function(u, nmax=52L){ # for 0 <= num < 1
  # 52 = num2dyadic(1-.Machine$double.eps)
  x <- u
  out <- integer(nmax)
  i <- 0L
  j <- 0L
  while(x>0 && i < nmax){
    j <- 1L + floor(-log2(x+.Machine$double.eps)) #floor(-log2(x)-.Machine$double.eps) #
    #i <- i + j
    #if(i <= nmax) out[i] <- 1L
    #x <- 2L^j*x - out[i]
    if(i+j <= nmax){
      i <- i + j
      out[i] <- 1L
      x <- 2L^j*x - 1L
    }else{
      i <- nmax
    }
  }
  if(!all.equal(u, sum(out[1:i]/2L^(1:i)))) stop("Incorrect result")
  return(as.dyadic.integer(out[1:i]))
}


