#' fftct2
#'
#' la fonction fftct2 implemente la transformee de Fourier de Cooley Tukey de base  en utlisant la methode diviser pour conquerir
#'
#' @param x un vecteur complexe
#'
#' @return le vecteur complexe correspondant à Xk.
#' @export
#'
#' @examples
#' fftct2(c(1+1i,4+7i,6+9i,20+3i))
#' fftct2(c(3+1i,7+4i))
#'
fftct2 <- function(x){
  identical(ceiling(log2(length(x))), floor(log2(length(x)))) ||
    stop("La longueur de x doit etre une puissance de 2")
  identical(length(x), 1L) && return(x)
  N <- length(x)
  z <- x
  {
    odd <- x[(1:(N/2))*2-1]
    even <- x[(1:(N/2))*2]
    z[1:(N/2)] <- fftct2(odd)
    z[((N/2)+1):N] <- fftct2(even)

    x[1:(N/2)] <- z[1:(N/2)] + exp(-1i*2*pi*(0:(N/2-1))/N) * z[(N/2+1):N]
    x[((N/2)+1):N] <- z[1:(N/2)] - exp(-1i*2*pi*(0:(N/2-1))/N) * z[(N/2+1):N]
  }
  return(x)
}
