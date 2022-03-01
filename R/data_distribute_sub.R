#' Title
#'
#' @param ledger ledger
#'
#' @export
#'
#' @import dplyr
#' @import lubridate
data_distribute_sub <- function(ledger){

  # rows with occurence other than 0 get repeated
  # amount is split by occurence
  # dates will be set to the future occurences
  rest <- ledger[ledger$occurence == 0, ]
  change <- ledger[ledger$occurence != 0, ]

  change <- change %>%
    mutate(amount = .data$amount / .data$occurence)

  date_v <- as.Date(NULL)

  for (i in 1:nrow(change)) {
    start <- change[i,]$date

    if(change[i,]$occurence > 0){
      end <- start %m+% months(change[i,]$occurence-1)

      tmp_date <- seq(from=start,
                      to=end,
                      by="month")
    } else {
      end <- start %m-% months(abs(change[i,]$occurence)-1)
      tmp_date <- seq(from=end,
                      to=start,
                      by="month")
    }


    date_v <- c(date_v, tmp_date)
  }

  result <- data.frame(lapply(change, rep, abs(change$occurence))) %>%
    mutate(
      occurence = 0,
      balance = NA
    )

  result$date <- date_v

  ledger <- rbind(rest, result)

  ledger <- ledger %>%
    mutate(date = ymd(date))

  return(ledger)

}
