#' dft1_iter
#'
#' dft1_iter implemente la version iterative de la transformee de Fourier discrete.
#'
#' @param k  un nombre naturel
#' @param x un vecteur complexe
#'
#' @return Xk le nombre complexe correspondant
#' @export
#'
#' @examples
#' dft1_iter(2,c(3+1i,7+4i))
#' dft1_iter(5,c(5+2i,9+1i))
#'
dft1_iter <- function(k, x) {
  Xk=0
  N <- length(x)
  for(xN in 0:(N - 1)){
    Xk = Xk + x[xN  + 1] * exp((1i*-2*pi*k*xN)/N)
  }
  return(Xk)
}
