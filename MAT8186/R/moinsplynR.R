#'Definition la methode Implémente la méthode moins(p, q)
#'de permettant de sosutraire les polynômes la classe plynR
#'
#' @param p objet de la classe plynR
#' @param q objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' q=plynR(coefs=c(1,4,7))
#' moins(p,q)
#' @export
moins<-function(p,q) UseMethod("moins")
#' @export
moins.plynR<-function(p,q){
  degrees<-c(max(seq_along(p)),max(seq_along(q)))
  if (max(degrees)==degrees[1]) {
    Polysum<-p-c(q,rep(0,(degrees[1]-degrees[2])))
  } else if (degrees[1]==degrees[2]) {
    Polysum<-p-q
  }else{
    Polysum<-c(p,rep(0,(degrees[2]-degrees[1])))-q
  }
}

