#' Title
#'
#' @param ledger short ledger as df
#' @param time_key date key in short ledger: year, quarter, month, date
#'
#' @export
#'
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @import scales
#'

net_plot <- function(ledger, time_key) {

  my_colors <- c("Income" = "#77DD76",
                 "Expense" = "#FF6962")

  net_data <- ledger %>%
    group_by(.data[[time_key]]) %>%
    summarise(amount = sum(.data$amount)) %>%
    mutate(type = ifelse(.data$amount > 0,"Income","Expense"))


  net_plot <- ggplot(net_data, aes(.data[[time_key]],
                                   .data$amount,
                                   fill = .data$type,
                                   label = dollar(.data$amount,
                                     prefix = "",
                                     suffix = "\U20AC",
                                     big.mark = ".",
                                     decimal.mark = ",",
                                     accuracy = 1)))+
    geom_bar(stat="identity")+
    theme_light()+
    labs(x = "", y = "", title = paste0("net income by ",time_key))+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )+
    scale_fill_manual(values = my_colors)+
    geom_label()+
    scale_y_continuous(labels = scales::dollar_format(prefix = "",
                                                      suffix = "\U20AC",
                                                      big.mark = ".",
                                                      decimal.mark = ","))

  switch (time_key,
          year = {net_plot <- net_plot+ scale_x_date(date_labels = "%Y", date_breaks = "1 year")},
          quarter = {net_plot <- net_plot+scale_x_date(date_labels = "%Y %b", date_breaks = "1 month")},
          month = {net_plot <- net_plot+scale_x_date(date_labels = "%Y %b", date_breaks = "1 month")}
  )

  return(net_plot)
}
