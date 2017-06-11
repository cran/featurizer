#' @name infer_gender
#' @title Infer gender for list of names fast
#' @description This function used a database of names and corresponding gender to lookup the gender of a name. It takes in a vector of names and returns a vector indicating gender for the name. The gender is denoted by "m"/"f"/"u" for male/female/unknown respectively.
#' @usage infer_gender(x, detect_cores)
#' @param x The vector of names to perform paralleized inference on
#' @param detect_cores If False (default), 2 cores are used. If True, half the number of cores are used on Mac OS else 2 cores are used.
#' @return The a vector with m/f/u if the name in the original vector was male female or Unknown respectively
#' @examples
#' infer_gender(x=c("abby", "Nick", "abc"))
#' @export infer_gender
#' @import parallel
#' @import utils

library(parallel)

infer_gender <- function(x, detect_cores=F){
  # takes is a vector of names
  # returns a vector of "f", "m" and "u" for female, male and unknown
  gender <- read.csv(system.file("extdata", "Gender/mf.txt",
                              package = "featurizer"),
                  header=FALSE, stringsAsFactors = F, sep = "\t")
  names(gender) <- c("name", "sex")

  x <- toupper(x)
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
  clusterExport(cl, "gender", envir=environment())
  x <- parSapply(cl, x, function(k) {
                        ifelse(length(gender[which(gender$name == k), ]$sex)!=0,
                               gender[which(gender$name == k), ]$sex, "u")})
  stopCluster(cl)
  return(x)
}


