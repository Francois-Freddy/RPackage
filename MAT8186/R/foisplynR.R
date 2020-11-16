#'fois
#'
#'la methode fois implémente la méthode fois(p, q)
#'de permettant de multiplier les polynômes la classe plynR.
#'
#' @param p objet de la classe plynR
#' @param q objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' q=plynR(coefs=c(1,4,7))
#' fois(p,q)
#' @export
fois<-function(p,q) UseMethod("fois")
#' @export
fois.plynR<-function(p,q){
  polymult=rep(0,max(seq_along(p))+max(seq_along(q))-1)
  for(i in 1:(max(seq_along(p))+max(seq_along(q))-1)){turnipi=i-1
  for(j in 1:max(seq_along(p))){turnipj=j-1;for(k in 1:max(seq_along(q))){turnipk=k-1
  if(turnipi-turnipj==turnipk) polymult[i]=polymult[i]+p[j]*q[k]}}}
}


