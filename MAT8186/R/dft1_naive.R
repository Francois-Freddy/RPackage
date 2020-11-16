
#' dft1_naive implemente la version naive de la transformee de Fourier discrete.
#'
#' @param k un nombre naturel
#' @param x un vecteur complexe
#'
#' @return Xk le nombre complexe correspondant
#' @export
#'
#' @examples
#' dft1_naive(2,c(3+1i,7+4i))
#' dft1_naive(5,c(5+2i,9+1i))
#'
dft1_naive <- function(k, x) {
  N <- length(x)
  xN <- 0:(N-1)
  Xk <- sum(x*exp(1i*-2*pi*k*xN/N))
  return(Xk)
}
















