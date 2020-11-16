#'Definition la methode print(x) pour la classe plynR permettant d’afficher
#'le polynôme x a l’écran en faisant appel à la fonction cat
#'
#' @param x objet de la classe plynR
#' @examples
#' x=plynR(coefs=c(2,5,3))
#' print(x)
#' @export
print<-function(x) UseMethod("print")
#' @export
print.plynR<-function(x){
  for (k in seq_along(x)){
    if(sign(x[k])<0)
      cat(x[k],"X^",k-1,sep="")
    else
      cat("+",x[k],"X^",k-1,sep="")
  }
  cat("\n")
}
