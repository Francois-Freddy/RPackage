#' rand_gen
#'
#' rand_gen retourne une fonction permettant de simuler des
#  réalisations d’une variable aléatoire discrète
#  en utilisant la méthode de la transformée inverse.
#'
#' @param x vecteur de nombres entiers de taille arbitraire
#' @param trans fonction de transformation
#'
#' @return sim fonction simulateur acceptant un seul argument n positif et tenant le compte de chacune des valeurs simulées
#' @export
#' @importFrom stats runif
#'
#' @examples
#' rand_gen(x=c(2,5,9,7,6),trans=identity)
#'
rand_gen <- function(x,trans=identity) {
  trans_vect <- Vectorize(trans)
  counter <- as.table(rep(0L, length(x)))
  names(counter) <- lapply(seq(0L, length.out = length(x)),FUN=trans)
  red<- function(f, x) {
    out <- x[1]
    for(i in seq(2, length(x))) {
      out <- f(out, x[i])
    }
    out
  }
  red=red(f=sum, x)
  redc= Reduce(f = "+", x , accumulate = TRUE)
  function(n) {
    ret <- lapply(sapply(runif(n), function(y) sum(y > redc*(red^-1))),FUN=trans)
    counter <<- counter + table(factor(ret, names(counter)))
    sim=unlist(ret)
  }
}
