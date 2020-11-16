#' nombres_dec  est une fonction pour calculez le plus de termes possibles compris entre −1e10 et 1e10 tel que pour chacun des
#' termes calculés, il soit possible de retrouver sa «décomposition» facilement et rapidement.
#'
#' @param chiffres liste de nombres
#' @param boundaries intervalle de valeurs admissibles
#' @param nb_iters nombre d'iterations
#'
#' @return une liste des listes de nombres,d'operateurs et de candidats
#' @export
#'@importFrom stats rmultinom
#'@importFrom utils  head tail
#' @examples
#' nombres_dec()

nombres_dec <- function(chiffres = c(2, 2, 5, 6, 6, 6, 7, 9),boundaries=c(-1e10,1e10),nb_iters = 1e5)

{

  op_match <- list(`+`, `-`, `*`, `/`, `^`)

  res <- replicate(n = nb_iters, simplify = FALSE, expr = {

    rgen <- function(n, size, prob)
    {
      if(length(n) > 1) n <- length(n)
      if(length(n) == 0 || as.integer(n) == 0) return(numeric(0))
      n <- as.integer(n)
      if(n < 0) stop("integer(n) can not be negative in rmultinomial")

      if(is.vector(prob) || (dim(prob)[1]) == 1) {
        if(length(size)==1) return(t(rmultinom(n,size,prob)))
        prob <- matrix(prob,nrow=1)
      }

      nrp <- nrow(prob)
      mnr <- min( max(nrp ,length(size)), n)
      ss  <- rep(size,length.out=mnr)

      if(nrp != mnr) prob <- matrix(t(prob),ncol=ncol(prob),nrow=mnr,byrow=TRUE)

      n1 <- n%/%mnr
      n2 <- n%%mnr

      res <- sapply(1:mnr,function(x) rmultinom(n1,ss[x],prob[x,]))
      res <- matrix(res,ncol=ncol(prob),byrow=TRUE)
      index <- as.vector(matrix(1:(mnr*n1),ncol=mnr,byrow=TRUE))
      res <- res[index,]

      if (n2 != 0){
        res <- rbind(res,t(sapply(1:n2,function(x) rmultinom(1,ss[x],prob[x,]))))
      }
      return(res)
    }

    perm.at.random=function(x, size, replace = FALSE, prob = NULL){
      if (length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 1) {
        if (missing(size))
          size <- x
        sample.int(x, size, replace, prob)
      }
      else {
        if (missing(size))
          size <- length(x)
        x[sample.int(length(x), size, replace, prob)]
      }
    }

    n <- perm.at.random((1:length(chiffres))[-1], 1)
    u=as.vector(c(rgen(1L, n - 1L, rep(1L, 3))))
    res <- as.numeric(perm.at.random(chiffres, n)[[1L]])
    toRemove <- logical(n - 1L)


    for (k in 1L:(n - 1L)) {
      chiffre.probable <-
        op_match[[c(rep(5L, u[[1L]]),perm.at.random(3L:4L, u[[2L]], replace = TRUE),perm.at.random(1L:2L, u[[3L]], replace = TRUE))[k]]](res, perm.at.random(chiffres, n)[[k + 1L]])
      if (identical(ceiling(chiffre.probable), floor(chiffre.probable)))
        res <- as.numeric(chiffre.probable)
      else
        toRemove[k] <- TRUE
    }
    list(x = res,chiffres = perm.at.random(chiffres, n)[c(TRUE, !toRemove)],
         ops = c(rep(5L, u[[1L]]),perm.at.random(3L:4L, u[[2L]], replace = TRUE),perm.at.random(1L:2L, u[[3L]], replace = TRUE))[!toRemove])
  })


  restrict<-function(x,f){
    n <- as.logical(unlist(lapply(x, f)))
    x[which(n)]
  }

  res <- restrict(x = res, f = function(t) t[["x"]] <= boundaries[2] & t[["x"]] >= boundaries[1])

  apply_modif<-function(x) t(sapply(x,rbind))

  xs <- as.numeric(apply_modif(res)[,1L])
  xs_order <- order(xs)
  xs <- xs[xs_order]

  doublons <- which(head(xs, -1L) == tail(xs, -1L)) + 1L
  n <- length(xs) - length(doublons)
  if (identical(which(head(xs, -1L) == tail(xs, -1L)) + 1L, integer(0L)))
    doublons <- -seq_along(xs)

  chiffres <- apply_modif(res)[,2L][xs_order]
  ops <- apply_modif(res)[,3L][xs_order]

  lapply(1L:n, function(k) list(x = xs[-doublons][k],chiffres = chiffres[-doublons][k][[1L]],ops = ops[-doublons][k][[1L]]))
}

