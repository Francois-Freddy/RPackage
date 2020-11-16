#' dft_factory implemente la transformee de Fourier discrete en faisant usage des versions soit,naive,iterative ou matricielle
#' @param F une fonction.
#' @param x un vecteur complexe.
#'
#' @return un vecteur complexe
#' @export
#'
#' @examples
#' dft_factory(dft1_naive,c(3+1i,7+4i))
#' dft_factory(dft1_iter,c(3+1i,7+4i))
#' dft_factory(dft1_matrix,c(3+1i,7+4i))
#'
dft_factory <- function(F,x) {return(sapply(0:(length(x) - 1),FUN=F,x=x ))}
