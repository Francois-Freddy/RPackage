#'derive
#'
#'cette fonction implémente la méthode derive
#'permettant de deriver les polynômes la classe plynR.
#'
#' @param p objet de la classe plynR
#' @examples
#' p=plynR(coefs=c(2,5,3))
#' derive(p)
#' @export
derive<-function(p) UseMethod("derive")
#' @export
derive.plynR<-function(p){ifelse(max(seq_along(p))!=1,return(plynR(((c(0:(max(seq_along(p))-1)))*p)[-1])),return(plynR(0)))}



