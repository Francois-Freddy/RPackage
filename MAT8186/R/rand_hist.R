#' rand_hist
#'
#' une fonction qui affiche l’histogramme des valeurs générées jusqu’à maintenant par un simulateur
#' @param sim un simulateur
#'
#' @return un histogramme des valeurs générées jusqu’à maintenant par un simulateur
#' @export
#' @importFrom graphics barplot
#'
#' @examples
#' sim=rand_gen(x=c(2,5,9,7,6),trans=identity)
#' sim(20)
#' rand_hist(sim)
rand_hist <- function(sim) {barplot(rand_stats(sim), space = 0L)}
