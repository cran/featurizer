#' @name par_sapply
#' @title A friendlier parSapply.
#' @description A sleek wrapper on the function parSapply. This function does not require user to allocate clusters and then stop them. This function uses parSapply across half the available cores and after the computation is done, it stops the clusters as well. The inputs are same os parSapply from parallel package
#' @usage par_sapply(X, FUNC, detect_cores)
#' @param X The vector of value to perform paralleized sapply on.
#' @param FUNC Function to be applied to every value of X, similar to sapply
#' @param detect_cores If False (default), 2 cores are used. If True, half the number of cores are used on Mac OS else 2 cores are used.
#' @return The a vector just like sapply. It does applies the function on X in parallel and hence is very fast.
#' @examples
#' par_sapply(1:100, function(x) x*100)
#' @export par_sapply
#' @import parallel

library(parallel)

par_sapply <- function(X, FUNC, detect_cores=F){
  # X is a vector on which FUNC is applied to
  # x is the resulting vector which is returned

  if(detect_cores==F){
    no_cores <- 2
  } else {
    if (tolower(as.character(Sys.info()['sysname'])) == "darwin"){
      no_cores <- detectCores(logical = TRUE)/2
    } else {
      no_cores <- 2
    }
  }
  cl <- makeCluster(no_cores)
  x <- parSapply(cl, X, function(k) FUNC(k))
  stopCluster(cl)
  x
}
