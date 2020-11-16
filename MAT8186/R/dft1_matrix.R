#' dft1_matrix implemente la version matricielle de la transformee de Fourier discrete en faisant usage de la fonction crossprod
#'
#' @param k un nombre naturel
#' @param x un vecteur complexe
#'
#' @return Une matrice sur la base d'un vecteur complexe x et un nombre naturel k.
#' @export
#'
#' @examples
#' dft1_matrix(2,c(3+1i,7+4i))
#' dft1_matrix (5,c(5+2i,9+1i))
#'
dft1_matrix <- function(k, x){as.matrix(as.complex(crossprod(x,exp((1i*-2*pi*k*(0:(length(x) - 1)))/(length(x))))))}
