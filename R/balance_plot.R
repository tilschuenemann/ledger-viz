#' Title
#'
#' @param ledger
#'
#' @export
#'
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import rlang
#'
balance_plot <- function(ledger){

  # TODO nuding is absolute, should be calculated
  # also x axis labels might be bad for some months
  # TODO title

  balance_data <- ledger %>%
    group_by(date) %>%
    summarise(balance_avg = mean(balance))

  first_label <- balance_data %>% slice(which.min(.data$date))
  last_label <- balance_data %>% slice(which.max(.data$date))

  balance_plot <- ggplot(balance_data, aes(date,balance_avg,label = dollar(.data$balance_avg,
                                                  prefix = "",
                                                  suffix = "\U20AC",
                                                  big.mark = ".",
                                                  decimal.mark = ",",
                                                  accuracy = 1
  )))+
    geom_line()+
    geom_point(aes(stroke=1))+
    theme_light()+
    labs(x="",y="",title = "balance for last month")+
    scale_y_continuous(limits = c(0,NA),labels = scales::dollar_format(prefix = "", suffix = "\U20AC", big.mark = ".", decimal.mark = ","))+
    scale_x_date(limits=c(start,end))+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )+
    geom_label(data=first_label, aes(date,balance_avg),nudge_y = 200)+
    geom_label(data=last_label, aes(date,balance_avg),nudge_y = 200)

  return(balance_plot)
}
