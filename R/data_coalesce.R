

#' Title
#'
#' @param ledger ledger
#' @param input_custom vector of columns to be coalesced with their custom counterpart
#'
#' @return ledger
#' @export
#'
#' @import rlang
#' @import dplyr
#' @import lubridate
data_coalesce <- function(ledger,input_custom){

  if (length(input_custom) != 0) {

    for (i in 1:length(input_custom)) {
      arg <- input_custom[i]
      custom_arg <- paste0(arg, "_custom")

      ledger <- ledger %>%
        mutate(!!arg := coalesce(ledger[[custom_arg]], ledger[[arg]]))
    }
  }

  # creation of date columns can only be done after coalescing all custom columns,
  # as date can also be customized

  ledger <- ledger %>%
    mutate(
      month = floor_date(date, "month"),
      quarter = floor_date(date, "quarter"),
      year = floor_date(date, "year")
    )

  return(ledger)
}
