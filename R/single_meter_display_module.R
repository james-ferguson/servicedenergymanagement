

regatta <- function(model_history){
  sails <-  model_history %>% arrange(ts)# %>%  filter(model_history, model)
  ud <- sign(sails$waste)
  len <- length(ud)
  sails$era = cumsum(1* (ud != c(ud[2:len], ud[len])))
  regatta <- group_by(sails, era) %>%
    group_modify(~cu(.x,.y))
}


cu <- function(df, .k){
  df$y = 24 * cumsum(df$waste)
  df
}

anomaly_chart <- function(sails){
  p <- chart(sails)
  for(i in 1:max(sails$era)){
    p <- p %>%
      add_lines(data = sails %>% filter(era==i), x = ~ts, y = ~y, alpha=0.6 , fill = "tozeroy", text = ~text, key= ~era, showlegend=FALSE)
  }

  p %>% layout(title = "", xaxis = list(title =  "") , yaxis = list(title = "Event Cost kWh"))
}





#' @export
single_meter_display_UI <- function(id){
  ns <- NS(id)

  box(
    title = tagList(span(shiny::icon("chart"), "Selected Meter Charts Meter: ", textOutput(ns("mpr"), inline = TRUE))),
    width = 12,
    fluidRow(
      column(
        width = 7,
        box(
          title = "Over Time",
          width = 12,
          plotlyOutput(ns("ts_plot"), height = 310),
          solidHeader = TRUE, background = "teal"
        ),
        box(
          title = "Cumulative Over Time",
          width = 12,
          plotlyOutput(ns("ts_cplot"), height = 310),
          solidHeader = TRUE, background = "teal"
        )
      ),
      column(
        width = 5,
        box(
          title = "Over Temperature",
          width = 12,
          plotlyOutput(ns("oat_plot"), height = 310),
          solidHeader = TRUE, background = "teal"
        ),
        box(
          title = "Forecast vs. Actual Day Profiles",
          width = 12,
          plotlyOutput(ns("fa_profile_plot"), height = 310),
          solidHeader = TRUE, background = "teal"
        )
      ),
      column(
        width = 12,
        box(
          width = 12,
          title = "Meter Anomaly History",
          plotlyOutput(ns("anomaly_plot"), height = 410),
          solidHeader = TRUE, background = "teal",
          collapsible = TRUE, collapsed = FALSE
        )
      )
    ),
    single_meter_detail_UI(ns('smd')),
    collapsed = FALSE,
    collapsible = TRUE

  )

}


#' @export
single_meter_display_server <- function(id){
  moduleServer(id, function(input, output, session) {

    meter <- reactive(req(session$userData$selected_meter()))

    mid <- reactive(req(meter()) %>% pull(mid))

    single_meter_detail_server('smd')

    selected_meter_history <- reactive(session$userData$selected_meter_history())

    selected_ts <- reactiveVal()

    hover <- selected_ts %>% debounce(100)

    observeEvent({
      selected_meter_history()
      event_data("plotly_hover", source = "day_selection_chart")
    }, {
      ed <- event_data("plotly_hover", source = "day_selection_chart")
      selected_ts(ed$key[1])
    })

    observeEvent(selected_meter_history(),{
      h <- selected_meter_history()
      valid <- filter(h, is.finite(kw), kw > 0)
      ts = max(valid$ts, na.rm=TRUE)
      selected_ts(ts)
    })

    selected_day <-  reactive({
      ts = as.Date(req(hover()))
      h <- selected_meter_history()
      day <- h[match(ts,h$ts),]
    })

    meter_pentiles <- reactive( mid() %>% meter_pentile_profiles() )

    fa_profile_chart = reactive({
      meter_pentiles <- meter_pentiles()
      day = selected_day()
      actual_profile <- mid() %>% day_profile(hover())
      forecast_actual_profile_chart(actual_profile, meter_pentiles, day)
    })

    star <- reactive({
      s <- hover()
      req(!is.null(s), !is.na(s), (length(s) ==1))
      acc %>% filter(ts == s)
    })

    bob <- reactive({
      smh <- req(selected_meter_history())
        select(smh, ts, kw, expected, dow, model, oat, waste) %>%
        arrange(oat)
    })

    acc <- SharedData$new(bob, ~ts)

    ts_chart  <- reactive(time_series_chart(acc, source = "day_selection_chart"))

    tsc_chart <- reactive(time_series_cumulative_chart(acc, source = "day_selection_chart"))

    oat_chart  <- reactive(outside_temperature_chart(acc, source = "day_selection_chart"))

    output$anomaly_plot <- renderPlotly({
      smh <- req(selected_meter_history())

    #  promise <- future(regatta(smh))

     # promise %...>%

      ch <- anomaly_chart(regatta(smh))

      ch

    })

    output$fa_profile_plot <- renderPlotly(fa_profile_chart())

    output$ts_plot <- renderPlotly(ts_chart())

    output$ts_cplot <- renderPlotly(tsc_chart())

    output$oat_plot <- renderPlotly(oat_chart())

  })

}
