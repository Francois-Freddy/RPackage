#' fib_mulPos_rec implemente la suite de Fibonnacci multiplicative positionnelle
# qui calcule le kieme terme de la suite de Fibonnacci multiplicative
# positionnelle
#'
#' @param k un nombre entier positif.
#'
#' @return  kieme terme de la suite de Fibonnacci multiplicative positionnelle.
#' @export
#'
#' @examples
#' fib_mulPos_rec(5)
#' fib_mulPos_rec(14)
#'
#'
fib_mulPos_rec <- function(k) {
  (k <= 2)&& return(k);
  return (k*fib_mulPos_rec(k - 1));
}


