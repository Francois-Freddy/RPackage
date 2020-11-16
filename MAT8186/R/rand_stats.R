##' rand_stats
#'
#'  rand_stats est une fonction retournant un vecteur contenant la proportion de chacun des nombres générés jusqu’à maintenant
#'
#' @param sim est un simulateur
#'
#' @return un vecteur contenant la proportion de chacun des nombres générés jusqu’à maintenant
#' @export
#'
#'
#' @examples
#' sim=rand_gen(x=c(2,5,9,7,6),trans=identity)
#' sim(20)
#' rand_stats(sim)
rand_stats <- function(sim) {proportions(environment(sim)[["counter"]])}
