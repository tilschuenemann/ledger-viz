library("shiny")
library("shinydashboard")
library("shinyjs")
library("readr")
library("dplyr")
library("ggplot2")
library("scales")
library("lubridate")
library("DT")
library("ledger2viz")

ui <- {
  sidebar <- dashboardSidebar(disable = T)
  header <- dashboardHeader(disable = T)
  body <- dashboardBody(
    useShinyjs(),
    fluidRow(
      box(
        title = "Controls",
        status = "primary",
        solidHeader = T,
        width = 12,
        collapsible = T,
        column(
          width = 4,
          fileInput("fileupload", "File Upload", multiple = F, accept = ".csv"),
          uiOutput("daterange"),
        ),
        column(
          width = 2, uiOutput("filter_label1"),
          uiOutput("filter_label2"),
          uiOutput("filter_label3"),
          uiOutput("filter_label1_custom"),
          uiOutput("filter_label2_custom"),
          uiOutput("filter_label3_custom")
        ),
        column(
          width = 2,
          uiOutput("filter_recipient"),
          uiOutput("filter_recipient_clean"),
          uiOutput("filter_recipient_clean_custom"),
          uiOutput("filter_occurence")
        ),
        column(width = 2,
               checkboxGroupInput("custom", "Use custom values for:",
          choices = c("date", "amount", "recipient", "label1", "label2", "label3"))),
        column(
          width = 2,
          checkboxInput("calculate_subscription", "Distribute subscription")
        ),
      )
    ),
    fluidRow(hidden(div(
      id = "contents",
      box(
        title = "Data",
        status = "success",
        solidHeader = T,
        width = 12,
        collapsible = T,
        plotOutput("net_plot"),
        plotOutput("io_plot"),
        plotOutput("balance_plot"),
        fluidRow(column(width = 4,
                        plotOutput("rank_label1")),
                 column(width = 4,
                        plotOutput("rank_label2")),
                 column(width = 4,
                        plotOutput("rank_label3"))
        ),
        fluidRow(column(width = 4,
                        plotOutput("top_expenses")),
                 column(width = 4,
                        plotOutput("top_income")),
                 column(width = 4,
                        )
        )
      )
    )))
  )


  ui <- dashboardPage(header, sidebar, body)
}

server <- function(input, output, session) {

  observeEvent(input$fileupload, {
    shinyjs::show("contents")
  })

  # base data used for creating filters after data manipulation
  base_data <- reactive({
    req(input$fileupload)

    ledger <- read_data(input$fileupload)

    if (input$calculate_subscription) {
      ledger <- data_distribute_sub(ledger)
    }

    ledger <- data_coalesce(ledger, input$custom)

    return(ledger)
  })

  # filtered data for plotting
  filtered_data <- reactive({
    # TODO using eval + parse is bad
    # TODO  create more elegant solution

    req(base_data())
    req(input$daterange)

    ledger <- base_data()

    filter_ledger <- function(ledger, arg) {
      filter_arg <- paste0("filter_", arg)
      filter <- paste0("input$", filter_arg)

      if (!is.null(eval(parse(text = filter)))) {
        ledger <- ledger %>%
          filter(ledger[[arg]] %in% input[[filter_arg]])
      }
      return(ledger)
    }

    ledger <- filter_ledger(ledger, "label1")
    ledger <- filter_ledger(ledger, "label2")
    ledger <- filter_ledger(ledger, "label3")
    ledger <- filter_ledger(ledger, "label1_custom")
    ledger <- filter_ledger(ledger, "label2_custom")
    ledger <- filter_ledger(ledger, "label3_custom")
    ledger <- filter_ledger(ledger, "recipient")
    ledger <- filter_ledger(ledger, "recipient_clean")
    ledger <- filter_ledger(ledger, "recipient_clean_custom")

    ledger <- ledger %>%
      filter(between(
        date,
        input$daterange[1],
        input$daterange[2]
      ))

    return(ledger)
  })

  # filters -----------------------------------------------------------------

  # create filters ui from uploaded ledger
  # function for creating UI element; creating unique choices from ledger column
  # TODO this can be done more elegantly

  create_select <- function(ledger, arg) {
    id <- paste0("filter_", arg)
    return(selectInput(id,
      arg,
      choices = sort(as.vector(unique(ledger[[arg]]))),
      multiple = T
    ))
  }

  output$filter_label1 <- renderUI({
    create_select(base_data(), "label1")
  })
  output$filter_label2 <- renderUI({
    create_select(base_data(), "label2")
  })
  output$filter_label3 <- renderUI({
    create_select(base_data(), "label3")
  })
  output$filter_label1_custom <- renderUI({
    create_select(base_data(), "label1_custom")
  })
  output$filter_label2_custom <- renderUI({
    create_select(base_data(), "label2_custom")
  })
  output$filter_label3_custom <- renderUI({
    create_select(base_data(), "label3_custom")
  })
  output$filter_recipient_clean <- renderUI({
    create_select(base_data(), "recipient_clean")
  })
  output$filter_recipient_clean_custom <- renderUI({
    create_select(base_data(), "recipient_clean_custom")
  })
  output$filter_recipient <- renderUI({
    create_select(base_data(), "recipient")
  })
  output$filter_occurence <- renderUI({
    create_select(base_data(), "occurence")
  })

  output$daterange <- renderUI({
    req(base_data())

    ledger <- base_data()

    start_date <- min(as.Date(ledger$date, format = "%d.%m.%Y"))
    end_date <- max(as.Date(ledger$date, format = "%d.%m.%Y")) %m+% months(12)

    dateRangeInput(
      inputId = "daterange",
      label = "Date Range",
      start = start_date,
      end = end_date,
      weekstart = 6
    )
  })

  # plot --------------------------------------------------------------------

  output$balance_plot <- renderPlot({
    req(filtered_data())
    balance_plot(filtered_data())
  })

  output$net_plot <- renderPlot({
    req(filtered_data())
    net_plot(filtered_data(), "month")
  })

  output$io_plot <- renderPlot({
    req(filtered_data())
    io_plot(filtered_data(), "month")
  })

  output$rank_label1 <- renderPlot({
    req(filtered_data())
    ranking_plot(filtered_data(), "label1")
  })

  output$rank_label2 <- renderPlot({
    req(filtered_data())
    ranking_plot(filtered_data(), "label2")
  })

  output$rank_label3 <- renderPlot({
    req(filtered_data())
    ranking_plot(filtered_data(), "label3")
  })

  output$top_expenses <- renderPlot({
    req(filtered_data())
    top_n_plot(filtered_data(), 10L, "Expense")
  })

  output$top_income <- renderPlot({
    req(filtered_data())
    top_n_plot(filtered_data(), 10L, "Income")
  })

}

shinyApp(ui, server)
