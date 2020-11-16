#' dft_iter  implemente la transformee de Fourier discrete naive en faisant usage du "constructeur dft_factory
#' de la version matricielle dft_matrix.
#'
#' @param x un vecteur complexe
#'
#' @return vecteur complexe correspondanst à (Xk)
#' @export
#'
#' @examples
#' dft_matrix (c(3+1i,7+4i))
#' dft_matrix (c(5+2i,9+1i))
#'
dft_matrix <- function(x) dft_factory(dft1_matrix, x)
