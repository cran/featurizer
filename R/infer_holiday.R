#' @name infer_holiday
#' @title Infer if a date is a fedral holiday
#' @description A function that returns the federal holiday observed on a given date. If there was no holiday observed, it returns None. The function takes in a list of dates, where each date is in the format YYYY-MM-DD and checks them against a database of federal holidays (2012-2020). It returns the holiday that was/will be observed on the date. If no holiday is observed on that date, it returns "None" corresponding to that date. This computation happens in parallel and hence is very fast.
#' @usage infer_holiday(date_lst, detect_cores)
#' @param date_lst a list of dates strings of the format YYYY-MM-DD
#' @param detect_cores If False (default), 2 cores are used. If True, half the number of cores are used on Mac OS else 2 cores are used.
#' @return Returns the kind of fedral holiday that was/will be observed on that date. If there was no holiday observed, it returns None.
#' It does the lookup in parallel and hence is very fast.
#' @examples
#' infer_holiday(date_lst = c("2020-12-25", "2020-09-07", "2020-09-08", "2016-01-18",
#'  "2016-01-19", "2016-01-15"))
#' @export infer_holiday
#' @import parallel
#' @import utils

library(parallel)

infer_holiday <- function(date_lst, detect_cores=F){
  #read all holdays data from 2012 to 2020
  #takes a list of dates and returns the "No_holiday" if the date is not
  # a holiday and returns the name of holiday if it is a holiday.
  # accepts strings of the format "%Y-%m-%d"
  holidays <- read.csv(system.file("extdata", "Holidays/list_of_holidays.csv",
                                   package = "featurizer"),
                        stringsAsFactors = F)
  names(holidays) <- c("index", "Date", "Holiday")
  holidays$Date <- as.Date(holidays$Date, "%Y-%m-%d")
  holidays <- holidays[, -1]

  date_lst <- as.Date(date_lst, "%Y-%m-%d")

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
  clusterExport(cl, "holidays", envir=environment())
  x <- parSapply(cl, date_lst,
                 function(k) ifelse(k %in% holidays$Date,
                                    holidays[which(holidays$Date == k),
                                             ]$Holiday,
                                    "None"))
  stopCluster(cl)
  return(x)
}
