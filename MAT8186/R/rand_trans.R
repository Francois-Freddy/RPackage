#'rand_trans
#'
#'rand_trans est une  fonction permettant d’appliquer une transformation à un simulateur produit
# par la fonction que vous avez programmé en (a) generee par rand_gen
#'
#' @param sim le simulateur genere par rand_gen
#' @param trans la transformation est une fonction bijective à image dans les entiers positifs
#'
#' @return une fonction (simulateur) permettant de simuler de la distribution originale de sim transformée
# par trans telle que le compte des valeurs simulées pour le nouveau simulateur soit initialisé à 0.
#' @export
#'
#'
#' @examples
#' rand_trans(rand_gen(x=c(2,5)),trans=identity)(3)
rand_trans <- function(sim, trans) {rand_gen(environment(sim)[["x"]],trans)}
