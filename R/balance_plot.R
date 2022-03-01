#' Title
#'
#' @param ledger ledger
#'
#' @export
#'
#' @import ggplot2
#' @import scales
#' @import dplyr
#' @import rlang
#'
balance_plot <- function(ledger){

  # TODO nudging is absolute, should be calculated
  # also x axis labels might be bad for some months
  # TODO title

  balance_data <- ledger %>%
    group_by(date) %>%
    summarise(balance_avg = min(.data$balance))

  first_label <- balance_data %>% slice(which.min(.data$date))
  last_label <- balance_data %>% slice(which.max(.data$date))

  balance_plot <-
  ggplot(balance_data, aes(.data$date,.data$balance_avg,label = dollar(.data$balance_avg,
                                                  prefix = "",
                                                  suffix = "\U20AC",
                                                  big.mark = ".",
                                                  decimal.mark = ",",
                                                  accuracy = 1
  )))+
    geom_line()+
    geom_point()+
    theme_light()+
    labs(x="",y="",title = "balance")+
    scale_y_continuous(limits = c(0,NA),labels = scales::dollar_format(prefix = "", suffix = "\U20AC", big.mark = ".", decimal.mark = ","))+
    scale_x_date(limits=c(first_label$date,last_label$date),
                 date_labels = "%Y %b", date_breaks = "1 month")+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )+
    geom_label(data=first_label, aes(.data$date,.data$balance_avg),nudge_y = 200)+
    geom_label(data=last_label, aes(.data$date,.data$balance_avg),nudge_y = 200)

  return(balance_plot)
}
