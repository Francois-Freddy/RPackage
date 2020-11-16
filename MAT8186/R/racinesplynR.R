#'Definition la methode Implémente la méthode racines.plynR
#'de permettant de trouver les racines les polynômes la classe plynR.
#'
#' @param p objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' racines(p)
#' @export
racines<-function(p) UseMethod("racines")
#' @export
racines.plynR<-function(p){polyroot(p)}

