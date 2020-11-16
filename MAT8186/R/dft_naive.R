#' dft_naive implemente la transformee de Fourier discrete naive en faisant usage du "constructeur dft_factory
#' de la version naive dft1_naive.
#'
#' @param x un vecteur complexe
#'
#' @return vecteur complexe correspondanst à (Xk)
#' @export
#'
#' @examples
#' dft_naive(c(3+1i,7+4i))
#' dft_naive(c(5+2i,9+1i))
#'
dft_naive <- function(x) {dft_factory(dft1_naive, x)}

