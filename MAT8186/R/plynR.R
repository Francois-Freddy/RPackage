#' plynR
#'
#' plynR casse plynR modélisant l’ensemble des polynômes en 𝑥 sur R en se servant d un constructeur
#'
#' @param coefs liste de coefficients du polynome
#'
#' @return objet de la classe plynR
#' @export
#'
#' @examples
#' plynR(coefs=c(2,5,3))
#' plynR(coefs=c(1,6,4))
plynR=function (coefs){
  a <- as.numeric(coefs)
  while ((la <- length(a)) > 1 && a[la] == 0) a <- a[-la]
  structure(a, class = "plynR")
}
