kWh_kg_CO2e_UK <- function(x)
  paste0(format(round(x*0.283),big.mark   = ","), " kg.CO2e")
kWh_kg_CO2g_UK <- function(x)
  paste0(format(round(x*0.185),big.mark   = ","), " kg.CO2g")

co2_calculations <- function(period, utility){
  kwh=list(
    actual = round(24 * sum(period$kw, na.rm=TRUE)),
    forecast =round( 24 *  sum(period$expected, na.rm=TRUE)), # ex fit
    waste = round(24 *  sum(period$kw-period$expected, na.rm=TRUE)) # ditto
  )
  co2_convertor(utility, kwh)
}

budget_co2_calculations <- function(budget_period, utility){
  kwh=list( budget = round(24 * sum(budget_period$budget, na.rm=TRUE)) )
  co2_convertor(utility, kwh)
}

co2_convertor <- function(utility, kwh){
  if(utility == "Gas"){
    conv <- kWh_kg_CO2g_UK
  } else if(utility == "Electricity"){
    conv <- kWh_kg_CO2e_UK
  } else{
    conv <- function(x) NA
  }
  list(kwh, co2 = plyr::llply(kwh, conv))
}


kWh_cash <- function(x, price, currency, digits = 0){
  paste0(currency, " ", format(round(x*price, digits), big.mark   = ","))
}


coloured <- function(string, v, negate = FALSE){
  if(negate)
    v = -v
  if(v>0)
    paste("<span style=\"color:red\">", string,"</span>")
  else
    paste("<span style=\"color:blue\">", string,"</span>")
}

ou_consolidation_exists <- function(owner_id, utility, pool){
  rows <- dbq("SELECT owner_id, utility FROM v1.owner_utility WHERE owner_id = ?owner_id AND utility = ?utility LIMIT 1;", owner_id = owner_id, utility = utility)
  nrow(rows) > 0
}

filter_history_by_period <- function(ouc, period){
  today <- req(max(ouc$ts))
  ys <- as.Date(paste(format(Sys.Date(), "%Y"), "01-01", sep = "-"))
  rms <- floor_date(today - months(1), "month")
  rmq <- floor_date(today - months(3), "month")
  tr <- switch(period,
               "1"={c(ys  , today)}, # YTD
               "2"={c(today - 366  , today)}, # RY
               "3"={c(rmq  , today)}, # RQ
               "4"={c(rms  , today)}, # RM
               "5"={c(today - 6  , today)}, # RW
               "6"={c(as.Date("2000-01-01")  , today)}) # ALL

  ouc %>% filter(ts >= tr[1], ts <= tr[2])
}

account_period_charts_UI <- function(id){
  ns <- NS(id)
  box(
    title = tagList(shiny::icon("chart"), "Portfolio Overview Charts"),
    width = 12,
    id="sortChartx",
    box(
      title = "Forecast to Actual History",
      width = 6,
      plotlyOutput(ns("owner_utility_history_plot"), height = 310),
      solidHeader = TRUE, background = "teal"
    ),
    box(
      title = "Outside Air Temperature by Day of Week",
      width = 6,
      plotlyOutput(ns("owner_utility_oat_plot"), height = 310),
      solidHeader = TRUE, background = "teal"
    ),
    box(
      title = "Error Spread",
      width = 6,
      plotlyOutput(ns("owner_utility_error_history_plot"), height = 310),
      solidHeader = TRUE, background = "teal"
    ),
    box(
      title = "Cumulative Deviations",
      width = 6,
      plotlyOutput(ns("owner_utility_cusum_plot"), height = 310),
      solidHeader = TRUE, background = "teal"
    ),
    sortable_js("sortChartx"),
    status = "success", solidHeader = TRUE, collapsible = FALSE)
}

account_period_UI2 <- function(id){
  ns <- NS(id)
  box(
    width = 12,
    title = "Period Context",
    fluidRow(

      column(width = 3, dateInput(ns("budget"), "Start of Financial Year", value = NULL)  ),
      column(width = 3, textInput(ns("currency"), "Display Currency", value = "£") ),
      column(width = 3, numericInput(ns("gas_price"), "Gas Price / kWh", value = 0.027, step = 0.01)),
      column(width = 3, numericInput(ns("electricity_price"), "Electricity Price / kWh", value = 0.085, step = 0.01))

    ),  status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE)
}


account_period_UI <- function(id){
  ns <- NS(id)
  ns_to <- function(n)
    textOutput(ns(n))
  ns_ui <- function(n)
    uiOutput(ns(n))

    box(
      width = 12,
      title = "Period Budget & Forecast vs Actual",
      fluidRow(column(width =2, selectInput(
        ns("period"),
        label = "",
        choices = list(
          "Year to Date" = 1,
          "Rolling Year" = 2,
          "Rolling Quarter" = 3,
          "Rolling Month" = 4,
          "Rolling Week" = 5,
          "All Periods" = 6
        ),
        selected = 2
      )),column(width =3,"Consumption"),column(width = 3,"Finance"), column(width = 3,"Carbon Impact"), column(width = 1,"Percent")),
      hr(),
      fluidRow(column(width = 2, p("Budget")), column(width = 3, ns_to("kwh_budget")), column(width = 3,ns_to("cash_budget")),column(width = 3,ns_to("carbon_budget")),column(width = 1, ns_to("pc_budget"))),
      hr(),
      fluidRow(column(width = 2, p("Short-term Forecast")),column(width = 3, ns_to("kwh_forecast")), column(width = 3,ns_to("cash_forecast")),column(width = 3,ns_to("carbon_forecast")),column(width = 1,ns_to("pc_forecast"))),
      fluidRow(column(width = 2, p("Actual")), column(width = 3, ns_to("kwh_actual")), column(width = 3,ns_to("cash_actual")),column(width = 3,ns_to("carbon_actual")),column(width = 1, ns_to("pc_actual"))),
      hr(),
      fluidRow(column(width = 2, p("Avoidable Waste")), column(width = 3, ns_ui("kwh_waste")), column(width = 3, ns_ui("cash_waste")), column(width = 3, ns_ui("carbon_waste")), column(width = 1, ns_ui("pc_waste"))),
      status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE)

}



account_period_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {

      ou_consolidation  <- reactive({
       # req(session$userData$oid)
        ouc <- req(session$userData$owner_consolidation())

       # ouc <- read_owner_utility_consolidation(req(session$userData$oid()), req(session$userData$utility()))
        if(nrow(ouc)==0)
          showModal(modalDialog(title="Utility Not Tracked !",
                                p("We have no consolidated data for ",req(session$userData$utility()) , session$userData$owner_name(), ".") ,
                                p("At the risk of being forward - Is this something to consider ?" ),
                                easyClose = TRUE))
        ouc
      })



      ou_budget_consolidation  <- reactive({
        req(session$userData$oid)
        read_owner_utility_budget_consolidation(req(session$userData$oid()), req(session$userData$utility()))
      })

      filtered_budget <- reactive({

        req(ou_budget_consolidation()) %>% filter_history_by_period(input$period)

      })

      budget_period <- reactive( budget_co2_calculations(req(filtered_budget()),req(session$userData$utility())) )

      ou_time_selected <- reactive(ou_consolidation() %>% filter_history_by_period(input$period) )

      acounting_period <- reactive({
        utility <- req(session$userData$utility())
        period <- ou_time_selected()
        cc <- co2_calculations(period, utility)
      })

      observe({
        input$gas_price
        input$electricity_price
      },

      )

      price <- reactive({
        u <- session$userData$utility()
        if(u == "Gas"){
          input$gas_price
        } else if(u == "Electricity"){
          input$electricity_price
        } else{
          0
        }
      })

      #### figures ####

      output$kwh_forecast <- renderText({
        ap <- acounting_period()
        format(ap[[1]]$forecast, big.mark = ",")
      })
      output$kwh_actual <- renderText({
        ap <- acounting_period()
        format(ap[[1]]$actual, big.mark = ",")
      })
      output$kwh_waste <- renderText({
        ap <- acounting_period()
        coloured(format(ap[[1]]$waste, big.mark = ","),ap[[1]]$waste)
      })

      output$carbon_forecast <- renderText({
        ap <- acounting_period()
        ap[[2]]$forecast
      })
      output$carbon_actual <- renderText({
        ap <- acounting_period()
        ap[[2]]$actual
      })
      output$carbon_waste <- renderText({
        ap <- acounting_period()
        coloured(ap[[2]]$waste, ap[[2]]$waste)
      })

      output$cash_forecast <- renderText({
        ap <- acounting_period()
        kWh_cash(ap[[1]]$forecast, price(), input$currency, digits = 0) #
      })
      output$cash_actual <- renderText({
        ap <- acounting_period()
        kWh_cash(ap[[1]]$actual, price(), input$currency, digits = 0)
      })
      output$cash_waste <- renderText({
        ap <- acounting_period()
        coloured(kWh_cash(ap[[1]]$waste, price(), input$currency, digits = 0),ap[[1]]$waste)
      })
      output$cash_budget <- renderText({
        bp <- budget_period()
        kWh_cash(bp[[1]]$budget, price(), input$currency, digits = 0)
      })

      output$kwh_budget <- renderText({
        bp <- budget_period()
        format(bp[[1]]$budget, big.mark = ",")
      })
      output$carbon_budget <- renderText({
        bp <- budget_period()
        bp[[2]]$budget
      })
      output$pc_budget <- renderText({
        bp <- budget_period()
        paste(round(100,1), " %")
      })

      output$pc_forecast <- renderText({
        ap <- acounting_period()
        bp <- budget_period()
        paste(round(100*ap[[1]]$forecast/ bp[[1]]$budget,1), " %")
      })
      output$pc_actual <- renderText({
        ap <- acounting_period()
        bp <- budget_period()
        paste(round(100*ap[[1]]$actual/ bp[[1]]$budget,1), " %")
      })
      output$pc_waste <- renderText({
        ap <- acounting_period()
        bp <- budget_period()
        coloured(paste(round(100*ap[[1]]$waste/ bp[[1]]$budget,1), " %"), ap[[1]]$waste)
      })

      #### end figures ####

      ouhp <- reactive(events::ouhp(req(ou_consolidation()), req(ou_time_selected())))
      ouoat <- reactive(events::ouoat(req(ou_consolidation()), req(ou_time_selected())))
      ouehp <- reactive(events::ouehp(req(ou_time_selected())))
      oucp <- reactive(events::oucp(req(ou_time_selected())))
      output$owner_utility_oat_plot <- renderPlotly(ouoat())
      output$owner_utility_error_history_plot <- renderPlotly(ouehp())
      output$owner_utility_cusum_plot <- renderPlotly(oucp())
      output$owner_utility_history_plot <- renderPlotly(ouhp())

      #### end charts ####
      }
  )
}
