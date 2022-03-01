
#' Title
#'
#' @param ledger ledger
#' @param cat_key cat_key
#'
#' @export
#'
#' @import lubridate
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import rlang
#' @importFrom stats reorder
ranking_plot <- function(ledger, cat_key) {


  my_colors <- c("Income" = "#77DD76",
                 "Expense" = "#FF6962")

  ranking_data <- ledger %>%
    group_by(.data[[cat_key]]) %>%
    summarise(amount = sum(.data$amount)) %>%
    mutate(type = ifelse(.data$amount > 0,"Income","Expense"))

  ranking_plot <- ggplot(ranking_data, aes(.data$amount,
                                           reorder(.data[[cat_key]],-.data$amount),
                                           fill=.data$type,
                                           label = dollar(.data$amount,
                                                          prefix = "",
                                                          suffix = "\U20AC",
                                                          big.mark = ".",
                                                          decimal.mark = ",",
                                                          accuracy = 1)))+
    geom_bar(stat="identity")+
    theme_light()+
    scale_x_continuous(labels = dollar_format(prefix = "",
                                                      suffix = "\U20AC",
                                                      big.mark = ".",
                                                      decimal.mark = ",")) +
    labs(x = "", y = "", title = paste0(cat_key," ranking"))+
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank()
    )+
    scale_fill_manual(values = my_colors)+
    geom_label()
  return(ranking_plot)


}
