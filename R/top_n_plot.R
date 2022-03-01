#' Title
#'
#' @param ledger ledger
#' @param n n
#' @param type type
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @import scales
#'
top_n_plot <- function(ledger, n=10L, type){

  my_colors <- c("Income" = "#77DD76",
                 "Expense" = "#FF6962")


  if(type=="Expense"){
    topn_data <- ledger %>%
      filter(.data$type == "Expense") %>%
      slice_min(.data$amount,n=n) %>%
      arrange(.data$amount) %>%
      mutate(recipient_order = paste0(row_number()," ",.data$recipient_clean),
             amount_abs = abs(.data$amount))

    plot_title <- "top 15 expenses"

  } else if(type=="Income"){
    topn_data <- ledger %>%
      filter(.data$type == "Income") %>%
      slice_max(.data$amount,n=n) %>%
      arrange(-.data$amount) %>%
      mutate(recipient_order = paste0(row_number()," ",.data$recipient_clean),
             amount_abs = abs(.data$amount))


    plot_title <- paste0("top ",n," income")

  }

  topn_plot <- ggplot(topn_data, aes(.data$amount_abs, reorder(.data$recipient_order, .data$amount_abs),
                                     fill = .data$type,
                                     label = dollar(.data$amount,
                                                    prefix = "", suffix = "\U20AC", big.mark = ".",
                                                    decimal.mark = ",", accuracy = 1)))+
  geom_bar(stat="identity")+
  theme_light()+
  geom_label()+
  labs(x="",y="",title=plot_title)+
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  )+
  scale_fill_manual(values = my_colors)+
  scale_x_continuous(labels = scales::dollar_format(prefix = "", suffix = "\U20AC", big.mark = ".", decimal.mark = ","))

  return(topn_plot)
}
