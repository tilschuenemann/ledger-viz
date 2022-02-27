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
          uiOutput("daterange")
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
        column(width = 2, checkboxGroupInput("custom", "Use custom values for:",
          choices = c("date", "amount", "recipient", "label1", "label2", "label3"),
        )),
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
        plotOutput("balance_plot")
      ),
      box(
        title = "Last Month",
        status = "primary",
        solidHeader = T,
        width = 12,
        collapsible = T,
        uiOutput("month_picker"),
        plotOutput("m_balance"),
        column(
          width = 6,
          plotOutput("m_tope"),
          plotOutput("m_net"),
          plotOutput("m_io")
        ),
        column(
          width = 6,
          plotOutput("m_topi"),
          plotOutput("m_rank_label2"),
          plotOutput("m_rank_label1")
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

  data_raw <- reactive({
    l_file <- input$fileupload
    req(l_file)

    suppressWarnings({
      ledger <- read_delim(l_file$datapath,
        ";",
        escape_double = FALSE, locale = locale(encoding = "UTF-8", decimal_mark = ","),
        trim_ws = TRUE, col_types = cols()
      )
    })


    ledger <- ledger %>%
      mutate(date = as.Date(date, format = "%d.%m.%Y"))

    return(ledger)
  })

  data_dis <- reactive({
    req(data_raw())

    ledger <- data_raw()

    if (input$calculate_subscription) {
      # rows with occurence other than 0 get repeated
      # amount is split by occurence
      # dates will be set to the future occurences
      rest <- ledger[ledger$occurence == 0, ]
      change <- ledger[ledger$occurence != 0, ]

      change <- change %>%
        mutate(amount = amount / occurence)

      date_v <- as.Date(NULL)

      for (i in 1:nrow(change)) {
        tmp_date <- seq(change[i, ]$date,
          by = "month",
          length.out = as.numeric(change[i, ]$occurence)
        )
        date_v <- c(date_v, tmp_date)
      }

      result <- data.frame(lapply(change, rep, change$occurence)) %>%
        mutate(
          occurence = 0,
          balance = NA
        )

      result$date <- date_v

      ledger <- rbind(rest, result)

      ledger <- ledger %>%
        mutate(date = ymd(date))
    }

    return(ledger)
  })



  # filters -----------------------------------------------------------------

  # create filters ui from uploaded ledger
  # function for creating UI element; creating unique choices from ledger column

  create_select <- function(ledger, arg) {
    id <- paste0("filter_", arg)
    return(selectInput(id,
      arg,
      choices = sort(as.vector(unique(ledger[[arg]]))),
      multiple = T
    ))
  }

  output$filter_label1 <- renderUI({
    create_select(data_raw(), "label1")
  })
  output$filter_label2 <- renderUI({
    create_select(data_raw(), "label2")
  })
  output$filter_label3 <- renderUI({
    create_select(data_raw(), "label3")
  })
  output$filter_label1_custom <- renderUI({
    create_select(data_raw(), "label1_custom")
  })
  output$filter_label2_custom <- renderUI({
    create_select(data_raw(), "label2_custom")
  })
  output$filter_label3_custom <- renderUI({
    create_select(data_raw(), "label3_custom")
  })
  output$filter_recipient_clean <- renderUI({
    create_select(data_raw(), "recipient_clean")
  })
  output$filter_recipient_clean_custom <- renderUI({
    create_select(data_raw(), "recipient_clean_custom")
  })
  output$filter_recipient <- renderUI({
    create_select(data_raw(), "recipient")
  })
  output$filter_occurence <- renderUI({
    create_select(data_raw(), "occurence")
  })

  output$daterange <- renderUI({
    req(data_raw())

    ledger <- data_raw()

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

  output$month_picker <- renderUI({
    req(data_coal())

    choices <- data_coal()$month %>%
      unique() %>%
      sort()

    names(choices) <- data_coal()$month %>%
      unique() %>%
      sort() %>%
      format("%Y %B")


    selectInput("month_picker", "Select month to review",
      choices = choices,
      selected = 1,
      width = "20%"
    )
  })


  # coalesce data -----------------------------------------------------------

  # coalesces all columns which have been checked in input$custom
  # after that columns relying on date get calculated

  data_coal <- reactive({
    req(data_dis())

    coalesce_ledger <- function(ledger, custom_input) {
      if (length(custom_input) != 0) {
        for (i in 1:length(custom_input)) {
          arg <- custom_input[i]
          custom_arg <- paste0(arg, "_custom")
          ledger <- ledger %>%
            mutate(!!arg := coalesce(ledger[[custom_arg]], ledger[[arg]]))
        }
      }
      return(ledger)
    }

    ledger <- coalesce_ledger(data_dis(), input$custom) %>%
      mutate(
        month = floor_date(date, "month"),
        quarter = floor_date(date, "quarter"),
        year = floor_date(date, "year")
      )

    return(ledger)
  })

  # filter data -------------------------------------------------------------

  data_fil <- reactive({
    req(data_coal())
    req(input$daterange)

    filter_ledger <- function(ledger, arg) {
      filter_arg <- paste0("filter_", arg)
      filter <- paste0("input$", filter_arg)

      if (!is.null(eval(parse(text = filter)))) {
        ledger <- ledger %>%
          filter(ledger[[arg]] %in% input[[filter_arg]])
      }
      return(ledger)
    }

    ledger <- data_coal()
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


  # plot --------------------------------------------------------------------

  output$balance_plot <- renderPlot({
    req(data_fil())
    balance_plot(data_fil())
  })

  output$net_plot <- renderPlot({
    req(data_fil())
    net_plot(data_fil(), "month")
  })

  output$io_plot <- renderPlot({
    req(data_fil())
    io_plot(data_fil(), "month")
  })

  # monthly plots -----------------------------------------------------------

  mondata <- reactive({
    req(data_fil())
    req(input$month_picker)

    ledger <- data_fil()
    ledger <- ledger %>%
      filter(month == input$month_picker)

    return(ledger)
  })


  output$m_net <- renderPlot({
    req(mondata())
    net_plot(mondata(), "month")
  })

  output$m_io <- renderPlot({
    req(mondata())
    io_plot(mondata(), "month")
  })

  output$dayo <- renderPlot({
    req(mondata())
    net_plot(mondata(), "date")
  })

  output$m_rank_label1 <- renderPlot({
    req(mondata())
    ranking_plot(mondata(), "label1")
  })

  output$m_rank_label2 <- renderPlot({
    req(mondata())
    ranking_plot(mondata(), "label2")
  })

  output$m_tope <- renderPlot({
    req(mondata())
    top_n_plot(mondata(), 15L, "Expense")
  })

  output$m_topi <- renderPlot({
    req(mondata())
    top_n_plot(mondata(), 15L, "Income")
  })

  output$m_balance <- renderPlot({
    req(mondata())
    balance_plot(mondata())
  })
}

shinyApp(ui, server)
