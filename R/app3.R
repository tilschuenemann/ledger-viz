library("shiny")
library("shinydashboard")
library("shinyjs")
library("readr")
library("dplyr")
library("lubridate")
library("DT")
library("ledger2viz")

ui <- {

  sidebar <- dashboardSidebar(sidebarMenu(id="start",
                                          menuItem("Start",tabName = "start"),
                                          menuItem("Monthly View",tabName = "monthly")))
  header <- dashboardHeader()
  body <- dashboardBody(
    tabItems(
      tabItem("start",
    useShinyjs(),
    fluidRow(
      box(title = "Controls",
          status = "primary",
          solidHeader = T,
          width = 12,
          collapsible = T,
          column(width = 4,
            fileInput("fileupload", "File Upload", multiple = F, accept = ".csv"),
            uiOutput("daterange")
          ),
         column(width = 2,uiOutput("filter_label1"),
                uiOutput("filter_label2")),
         column(width =2,
                uiOutput("filter_label3"),
                uiOutput("filter_recipient")),
         column(width = 4,checkboxGroupInput("custom","Use custom values for:",
                                             choices = c("date","amount","recipient","label1","label2","label3"),
                                             #selected = c("date","amount","recipient","label1","label2","label3")
                                             )),
          )),

    hidden(div(id="contents",
    fluidRow(
    box(title = "Data",
        status = "primary",
        solidHeader = T,
        width = 12,
        collapsible = T,
        dataTableOutput("m_data"),
        plotOutput("net_plot"),
        plotOutput("io_plot")
        ))))),
    tabItem("monthly",
            fluidRow(
              box(title = "Last Month",
                                       status = "primary",
                                       solidHeader = T,
                                       width = 12,
                                       collapsible = T,
                                       uiOutput("month_picker"),
                                       plotOutput("m_balance"),
                                       column(width = 6,
                                              plotOutput("m_tope"),
                                              plotOutput("m_net"),
                                              plotOutput("m_io")),
                                       column(width = 6,
                                              plotOutput("m_topi"),
                                              plotOutput("m_rank_label2"),
                                              plotOutput("m_rank_label1"))
                  )
                  )
            ))
    )

  ui <- dashboardPage(header, sidebar, body)
}

server <- function(input, output, session) {

  observeEvent(input$fileupload,{
    shinyjs::show("contents")
  })


  ledger <- reactive({
    l_file <- input$fileupload
    req(l_file)

    suppressWarnings({
      ledger <- read_delim(l_file$datapath,
                           ";",
                           escape_double = FALSE, locale = locale(encoding = "UTF-8", decimal_mark = ","),
                           trim_ws = TRUE, col_types = cols()
      )
    })
    return(ledger)
  })

# filters -----------------------------------------------------------------

  output$daterange <- renderUI({
    req(ledger())

    ledger <- ledger()

    start_date <- min(ledger$date)
    end_date <- max(ledger$date)

    dateRangeInput(
      inputId = "daterange",
      label = "Date Range",
      start = start_date,
      end = end_date,
      weekstart = 6
    )

  })

  output$filter_label1 <- renderUI({
    req(ledger())

    selectInput("filter_label1",
                "label1",
                choices = sort(as.vector(unique(ledger()$label1))),
                multiple = T)
  })

  output$filter_label2 <- renderUI({
    req(ledger())

    selectInput("filter_label2",
                "label2",
                choices = sort(as.vector(unique(ledger()$label2))),
                multiple = T)
  })

  output$filter_label3 <- renderUI({
    req(ledger())

    selectInput("filter_label3",
                "label3",
                choices = sort(as.vector(unique(ledger()$label3))),
                multiple = T)
  })

  output$filter_recipient <- renderUI({
    req(ledger())


    if("recipient" %in% input$custom){
      selectInput("filter_recipient",
                  "recipient_clean",
                  choices = sort(as.vector(unique(ledger()$recipient_clean))),
                  multiple = T)
    } else {
      selectInput("filter_recipient",
                  "recipient",
                  choices = sort(as.vector(unique(ledger()$recipient))),
                  multiple = T)
    }
    })


  output$month_picker <- renderUI({
    req(ledger_coalesced())

    choices <- ledger_coalesced()$month %>%
      unique() %>%
      sort()


    names(choices) <- ledger_coalesced()$month %>%
      unique() %>%
      format("%Y %m") %>%
      sort()

    selectInput("month_picker", "Select month to review",
                choices = choices,
                selected = 1)
  })


# coalesce data -----------------------------------------------------------

  ledger_coalesced <- reactive({
    req(ledger())

   ledger <- ledger()

   ### coalesce values
   if ("amount" %in% input$custom) {
     ledger <- ledger %>%
       mutate(amount = coalesce(amount_custom, amount))
   } else if ("date" %in% input$custom) {
     ledger <- ledger %>%
       mutate(
         date_temp = coalesce(date_custom, date),
         month = floor_date(date_temp, "month"),
         quarter = floor_date(date_temp, "quarter"),
         year = floor_date(date_temp, "year")
       ) %>%
       select(-date_temp)
   } else if (!("date" %in% input$custom)) {
     ledger <- ledger %>%
       mutate(
         month = floor_date(date, "month"),
         quarter = floor_date(date, "quarter"),
         year = floor_date(date, "year")
       )
   } else if ("recipient" %in% input$custom) {
     ledger <- ledger %>%
       mutate(recipient_clean = coalesce(recipient_clean_custom, recipient_clean))
   } else  if ("label1" %in% input$custom) {
     ledger <- ledger %>%
       mutate(label1 = coalesce(label1_custom, label1))
   } else if ("label2" %in% input$custom) {
     ledger <- ledger %>%
       mutate(label2 = coalesce(label2_custom, label2))
   } else if ("label3" %in% input$custom) {
     ledger <- ledger %>%
       mutate(label3 = coalesce(label3_custom, label3))
   }
   return(ledger)
  })

# filter data -------------------------------------------------------------

  ledger_filtered <- reactive({
    req(ledger_coalesced())
    req(input$daterange)

    ledger <- ledger_coalesced()

    if(!is.null(input$filter_label1)){
      ledger <- ledger %>%
        filter(ledger$label1 %in% input$filter_label1)
    } else if(!is.null(input$filter_label2)){
      ledger <- ledger %>%
        filter(ledger$label2 %in% input$filter_label2)
    } else if(!is.null(input$filter_label3)){
      ledger <- ledger %>%
        filter(ledger$label3 %in% input$filter_label3)
    } else if(!is.null(input$filter_recipient)){
      ledger <- ledger %>%
        filter(ledger$recipient_clean %in% input$filter_recipient)
    }

   start <- input$daterange[1]
   end <- input$daterange[2]

   ledger <- ledger %>%
     filter(between(date,start,end))

   return(ledger)
  })


# plot --------------------------------------------------------------------

  output$net_plot <- renderPlot({
    req(ledger_filtered())
    net_plot(ledger_filtered(), "month")
  })

  output$io_plot <- renderPlot({
    req(ledger_filtered())
    io_plot(ledger_filtered(), "month")
  })

# monthly plots -----------------------------------------------------------

 mondata <- reactive({
   req(ledger_filtered())
   req(input$month_picker)

   ledger <- ledger_filtered()
   ledger <- ledger %>%
     filter(month == input$month_picker)

   return(ledger)
 })


  output$m_net <- renderPlot({
    req(mondata())
    net_plot(mondata(),"month")
  })

  output$m_io <- renderPlot({
    req(mondata())
    io_plot(mondata(),"month")
  })

 output$dayo <- renderPlot({
   req(mondata())
   net_plot(mondata(),"date")
   })

 output$m_rank_label1 <- renderPlot({
   req(mondata())
   ranking_plot(mondata(),"label1")
 })

 output$m_rank_label2 <- renderPlot({
   req(mondata())
   ranking_plot(mondata(),"label2")
 })

 output$m_tope <- renderPlot({
   req(mondata())
   top_n_plot(mondata(),15L,"Expense")
 })

 output$m_topi <- renderPlot({
   req(mondata())
   top_n_plot(mondata(),15L,"Income")
 })

 output$m_data <- renderDataTable({
 req(mondata())

   mondata()
 })

 output$m_balance <- renderPlot({
   req(mondata())
   balance_plot(mondata())
 })

}

shinyApp(ui, server)
