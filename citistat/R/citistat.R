library(devtools)
library(roxygen2)

#' Format the Dates Properly
#'
#' For the other functions in this system to work properly,
#' the table of Service Requests should have well-formatted dates.
#' This function takes the duedate, statusdate, and createdate and
#' mutates them into date functions. This is useful because excel
#' exports of SRs are frequently formatted as text or otherwise
#' not fit for R functions.
#'
#' @param duedate A date
#' @param statusdate A date
#' @param createdate A date
#' @return Proper format for duedate, statusdate, and createdate
#' @export
#' @examples
#' SRdate()

SRdate <- function(dataframe,duedate,statusdate,createdate){
  df <- dataframe
  df %>%
    mutate(as.Date(duedate)) %>%
    mutate(as.Date(statusdate)) %>%
    mutate(as.Date(createdate))
}

#' Create a Backlog Table
#'
#' This function takes all SRs created within a certain timeframe and
#' creates three sets of tables for a selected date. The first table, "od",
#' lists the number of SRs that were open and overdue on a weekly basis from
#' the start date to the end date. The second table, "op", lists the number of
#' SRs that were open (overdue or otherwise) on a weekly basis from the start
#' to the end date. The third, "ov", lists the percent of SRs that were overdue
#' each week within that interval.
#'
#' This type of analysis is useful because it shows how much work an agency needs
#' to do to catch up to resident needs. It should be viewed alongside the percent
#' on time, which is easier to calculate but less descriptive of resident needs.
#' A long-running backlog is more concerning than an agency that misses its SR targets
#' by a day or two but nonetheless completes the work.
#'
#' This function creates a table, dates, from the start to end date selected.
#'
#' It also creates a function, "oover", that calculates the number of SRs overdue
#' at any given date. "oopen" does the same thing for the number open, and "poover"
#' calculates the percent overdue. These three functions can be used for any date using
#' the SR dataset or any other time-bound work.
#'
#'@param dataframe The SR or other time-bound workflow dataset
#'@param start The start date for the tables, op, od, and ov
#'@param end The end date for the tables, op, od, and ov
#'@return Three tables of dates and values
#' @export
#' @examples
#' citistatSRs()

citistatSRs <- function(dataframe,start,end){

  df <- dataframe
  dates <- seq.Date(from = as.Date(start),
                    to = as.Date(end),
                    by = '7 days')

  ##Create function that shows the overdue SRs over time
  oover <- function(arg1){
    NROW(df$`SR ID`[which(
      df$`Due Date` < arg1&
        ((df$`SR Status` == "Closed" &
            df$`Status Date` > arg1)|
           df$`SR Status` == "Open"))])
  }

  ##Create a function for the total number of open SRs at a given date
  oopen <- function(arg2){
    NROW(df$`SR ID`[which(
      df$`Created Date` < arg2&
        ((df$`SR Status` == "Closed" &
            df$`Status Date` > arg2)|
           df$`SR Status` == "Open"))])
  }

  ##Create a function for the percent of open SRs that were overdue at a given date
  poover <- function(arg3){oover(arg3)/oopen(arg3)}

  ##Create a Data Frame for the dates and values for each of the above functions
  od <- data.frame("Date" = dates, "Overdue" = sapply(dates,oover))
  op <- data.frame("Date" = dates, "Number Open" = sapply(dates,oopen))
  ov <- data.frame("Date" = dates, "Percent Open" = sapply(dates,poover))

}

devtools::check()

