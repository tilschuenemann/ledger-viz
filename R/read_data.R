
#' Title
#'
#' @param input_fileupload Shiny fileUpload reference
#'
#' @return dataframe
#' @export
#'
#' @import readr
read_data <- function(input_fileupload){

  var <- input_fileupload

  # TODO use vroom

  suppressWarnings({
    ledger <- read_delim(var$datapath,
                         ";",
                         escape_double = FALSE,
                         locale = locale(encoding = "UTF-8", decimal_mark = ","),
                         trim_ws = TRUE,
                         col_types = cols()
    )
  })

  return(ledger)

}
