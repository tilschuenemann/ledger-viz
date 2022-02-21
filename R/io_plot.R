#' Title
#'
#' @param ledger ledger
#' @param time_key see other
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import rlang
#'
io_plot <- function(ledger, time_key){

  my_colors <- c("Income" = "#77DD76",
                 "Expense" = "#FF6962")

  io_data <- ledger %>%
    group_by(.data[[time_key]], .data$type) %>%
    summarise(amount = sum(.data$amount),
              amount_abs = abs(amount)) %>%
    mutate(type = ifelse(.data$amount > 0,"Income","Expense"))


  io_plot <- ggplot(io_data, aes(.data[[time_key]], .data$amount_abs, fill = .data$type,
                                 label = dollar(.data$amount,
                                                prefix = "",
                                                suffix = "\U20AC",
                                                big.mark = ".",
                                                decimal.mark = ",",
                                                accuracy = 1
  )))+
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    geom_label(position = position_dodge2(width = 1)) +
    theme_light()+
    labs(x = "", y = "", title = paste0("income / expenses by ",time_key))+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )+
    scale_fill_manual(values = my_colors)+
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "\U20AC", big.mark = ".", decimal.mark = ","))

  switch (time_key,
          year = {io_plot <- io_plot+ scale_x_date(date_labels = "%Y", date_breaks = "1 year")},
          quarter = {io_plot <- io_plot+scale_x_date(date_labels = "%b", date_breaks = "1 month")},
          month = {io_plot <- io_plot+scale_x_date(date_labels = "%b", date_breaks = "1 month")}
  )

  return(io_plot)

}
