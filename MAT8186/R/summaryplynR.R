#'Definition la methode Implémente la méthode summary.plynR.
#'de permettant de trouver les racines les polynômes la classe plynR.
#'
#' @param p objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' racines(p)
#' @export
summary<-function(p) UseMethod("summary")
#' @export
summary.plynR<-function(p){
  cat(cat("p="),print(p))
  cat("factorisation","\n")
  for(k in 1:(max(seq_along(p))-1)){
    cat("(x-(",racines(p)[k],"))","")
  }
  cat("\n")
}
