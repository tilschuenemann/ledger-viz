

#' Title
#'
#' @param short_ledger sl df
#' @param time_key see
#' @param future_net future net amount per month
#'
#' @export
#'
#' @importFrom lubridate floor_date
#' @importFrom lubridate %m+%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 stat_smooth
#' @importFrom scales dollar_format
#' @importFrom rlang .data

hist_plot <- function(short_ledger, time_key, savings){

  my_colors <- c("Income" = "#77DD76",
                 "Expense" = "#FF6962")

  # get first and last month  of prediction
  pred_start <- floor_date(max(short_ledger$date), unit = "month") %m+% months(1)
  pred_end <- pred_start %m+% months(12)

  pred_df <- data.frame(
    time_key_var = seq.Date(from = pred_start, pred_end, by = time_key),
    amount = savings,
    origin = "prediction"
  )

  hist_data <- short_ledger %>%
    group_by(.data[[time_key]]) %>%
    summarise(balance = mean(.data$balance)) %>%
    mutate(origin = "inventory") %>%
    rename(time_key_var = 1)

  pred_df$amount[1] <- pred_df$amount[1] + hist_data$balance[nrow(hist_data)]
  pred_df$balance <- cumsum(pred_df$amount)
  pred_df$amount <- NULL


  hist2_data <- rbind(hist_data,pred_df)

  hist_plot <- ggplot(hist2_data, aes(.data$time_key_var, .data$balance))+
    geom_line(aes(linetype = .data$origin))+
    geom_point()+
    theme_light()+
    scale_y_continuous(limits = c(0, NA),labels = scales::dollar_format(prefix = "", suffix = "\U20AC", big.mark = ".", decimal.mark = ",")) +
    labs(x = "", y = "", title = paste0("history and 12 month prediction for savings of ",savings,"\U20AC"))+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )+
    stat_smooth(method = "loess",formula = "y ~ x",geom="line", alpha=0.3, size=3,data = hist_data)

  switch (time_key,
          year = {hist_plot <- hist_plot+ scale_x_date(date_labels = "%Y", date_breaks = "1 year")},
          quarter = {hist_plot <- hist_plot+scale_x_date(date_labels = "%b", date_breaks = "1 month")},
          month = {hist_plot <- hist_plot+scale_x_date(date_labels = "%b", date_breaks = "1 month")}
  )

  return(hist_plot)
}
