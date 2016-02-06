#' @name odometer
#' @rdname odometer
#' @title The odometer transformation
#' @description Applies the odometer transformation
#' @param x a dyadic number or a real number between 0 (included) and 1 (excluded)
#' @param image \code{"forward"} to apply the odometer, \code{"backward"} to apply its inverse
#' @param niters the number of iterations
#' @return the image by the odometer
NULL

#' @rdname odometer
#' @export
odometer <- function(x, image=c("forward", "backward"), niters=1L) UseMethod("odometer")

#' @rdname odometer
odometer0.dyadic <- function(d, image=c("forward", "backward")){
  image <- match.arg(image)
  if(image=="forward"){
    if(all(d==1L)){
      d <- c(rep(0L, length(d)), 1L)
    }else{
      k <- which.min(d)
      d[1:k] <- 1-d[1:k]
    }
    return(as.dyadic.integer(d))
  }
  if(image=="backward"){
    if(all(d==0L)){
      d <- c(rep(1L, length(d)), 0L)
    }else{
      k <- which.max(d)
      d[1:k] <- 1-d[1:k]
    }
    return(as.dyadic.integer(d))
  }
}

#' @rdname odometer
#' @export
odometer.dyadic <- function(x, image=c("forward", "backward"), niters=1L){
  image <- match.arg(image)
  d <- as.dyadic(x)
  if(image=="forward"){
    d <- odometer0.dyadic(d, image="forward")
    if(niters==1L) return(d)
    nmax <- length(d)
    out <- matrix(0L, nrow=niters, ncol=max(c(52, nmax)))
    out[1, seq_along(d)] <- d
    for(i in 2:niters){
      if(all(d==1L)){
        nmax <- nmax+1L
        if(nmax>=ncol(out)) out <- cbind(out, 0L)
      }
      d <- odometer0.dyadic(d, image="forward")
      out[i, seq_along(d)] <- d
    }
    return(out[, 1:nmax])
  }
  if(image=="backward"){
    d <- odometer0.dyadic(d, image="backward")
    if(niters==1L) return(d)
    nmax <- length(d)
    out <- matrix(0L, nrow=niters, ncol=max(c(52, nmax)))
    out[1, seq_along(d)] <- d
    for(i in 2:niters){
      if(all(d==0L)){
        nmax <- nmax+1L
        if(nmax>=ncol(out)) out <- cbind(out, 0L)
      }
      d <- odometer0.dyadic(d, image="backward")
      out[i, seq_along(d)] <- d
    }
    return(out[, 1:nmax])
  }
}

#' @rdname odometer
#' @export
odometer.numeric <- function(x, image=c("forward", "backward"), niters=1L){
  if(x<0 || x>=1) stop("x must be in [0,1[")
  if(niters==1L){
    return(dyadic2num(odometer0.dyadic(num2dyadic(x), image=image)))
  }
  else{
    iters <- odometer.dyadic(num2dyadic(x), image=image, niters=niters)
    return(apply(iters, 1, dyadic2num))
  }
}
