#'Definition la methode Implémente la méthode plus(p, q)
#'de permettant d’additionner les polynômes la classe plynR
#'
#' @param p objet de la classe plynR
#' @param q objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' q=plynR(coefs=c(1,4,7))
#' plus(p,q)
#' @export
plus<-function(p,q) UseMethod("plus")
#' @export
plus.plynR<-function(p,q){
  degrees<-c(max(seq_along(p)),max(seq_along(q)))
  if (max(degrees)==degrees[1]) {
    Polysum<-p+c(q,rep(0,(degrees[1]-degrees[2])))
  } else if (degrees[1]==degrees[2]) {
    Polysum<-p+q
  }else{
    Polysum<-c(p,rep(0,(degrees[2]-degrees[1])))+q
  }
}

