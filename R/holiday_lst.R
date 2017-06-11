#' @name holiday_lst
#' @title Fedral Holidays DataFrame
#' @description This is a simple function that returns a data frame of federal holidays and date on which they were observed. It has only 2 columns, Date and Holiday for years 2012-2020.The functions does not take any parameter but returns a data frame.
#' @usage holiday_lst()
#' @return Returns a data frame if fedral holidays and when they were observed from 2012 - 2020
#' @examples
#' holiday_lst()
#' @export holiday_lst
#' @import utils

holiday_lst <- function(){
  # read all holdays data from 2012 to 2020
  # returns a list of holidays
  holidays <- read.csv(system.file("extdata", "Holidays/list_of_holidays.csv",
                                    package = "featurizer"),
                       stringsAsFactors = F)
  names(holidays) <- c("index", "Date", "Holiday")
  holidays$Date <- as.Date(holidays$Date, "%Y-%m-%d")
  holidays <- holidays[, -1]
  return(holidays)
}
