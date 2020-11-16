#'  rand_sum
#'
#'une fonction permettant de faire la somme de deux simulateurs et
#'simulant des données proportionnellement aux vecteurs x1 et x2 respectivement et tel que Si x1 et x2
#'ne sont pas de la même longueur, on supposera que les valeurs absentes sont 0.
#'
#' @param sim1 un simulateur
#' @param sim2 un simulateur
#'
#' @return un simulateur  simulant proportionnellement à x1 + x2.
#' @export
#'
#' @examples
#' sim1=rand_gen(x=c(2,5))
#' sim2=rand_gen(x=c(1,7,9))
#' rand_sum(sim1,sim2)
rand_sum <- function(sim1,sim2) {
  xs <- lapply(list(sim1,sim2), function(s) environment(s)[["x"]])
  n <- Reduce(max, sapply(xs, length))
  new_x <- Reduce(x = xs, init = numeric(n),
                  f = function(x, y) {
                    y <- c(y, numeric(n - length(y)))
                    x + y
                  })
  rand_gen(new_x)
}
