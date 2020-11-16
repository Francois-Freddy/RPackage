#' Fonction qui gere l'affichage
#'
#' @param nb un nombre
#'
#' @return liste de nombres et une de leurs decompositions
#' @examples
#' decode(nombres_dec()[[1]])

#' @export
decode <- function(nb) {
  op_match <- list("+", "-", "*", "/", "^")
  cat(nb[["x"]], " = ", sep = "")
  cat(nb[["chiffres"]][[1L]])
  for (k in seq_along(nb[["ops"]]))
    cat(" ",
        op_match[[nb[["ops"]][k]]],
        " ",
        nb[["chiffres"]][k + 1L],
        sep = "")
  cat("\n")
}


res <- nombres_dec()
for (n in sample(res, 10, replace = TRUE))
  decode(n)
