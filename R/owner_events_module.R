owner_events_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'EVENTS',
    h2("Events"),
    fluidRow(
      column(width = 3,
             event_choice_UI(ns('ecm'))
             ),
      column(width = 2,  downloadButton(ns("event_report"), "Download report")),
      column(width = 2, radioButtons(ns('format'), 'Document format (currently only html functions)', c('HTML', 'Word','PDF'), inline = TRUE) ),
      column(width = 2, textInput(ns('credit'), 'Credit On Behalf of ...', placeholder = "eg TEC"))
    ),
    fluidRow(
      box(
        title = "Actual vs. Expected",
        width = 3,
        plotly::plotlyOutput(ns("a_vs_e_plot"), height = 350)
      ),
      box(
        title = "Distribution of Turndown/Excess",
        width = 3,
        plotly::plotlyOutput(ns("ae_dist_plot"), height = 350)
      ),
      tabBox(
        title = tagList(shiny::icon("chart"), ""),
        width =6,
        tabPanel("Consumption History", plotly::plotlyOutput(ns("owner_utility_history_plot"), height = 400), value = "Consumption Chart"),
        tabPanel("Forecast Accuracy", plotly::plotlyOutput(ns("owner_utility_error_history_plot"), height = 400), value = "Error Chart"),
        tabPanel("Cumulative Deviation", plotly::plotlyOutput(ns("owner_utility_cusum_plot"), height = 400), value = "Era Chart")
      )
    ),

    single_row_selector_ui(ns("meter_events"))
  )

}

owner_events_server <- function(id, owner, utility, ap){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {

    # Event Choice Module ####
    event_data <- event_choice_server('ecm', owner, utility)
    # Event Specific Charts ####

    observeEvent({event_data()},{
      data <- req(event_data())
      output$a_vs_e_plot <- renderPlotly(data$meter_events_summaries  %>% a_vs_e_chart(s =  session$userData$selected_meter()))
      output$ae_dist_plot <- renderPlotly(data$meter_events_summaries  %>% ae_dist_chart())
      output$owner_utility_error_history_plot <- renderPlotly(data$event_consolidation %>% high_low_forecast_errors() %>% maybe_show_event(data$event))
      output$owner_utility_cusum_plot <- renderPlotly(data$event_consolidation %>% cumulative_waste_impact_chart() %>% maybe_show_event(data$event))
      output$owner_utility_history_plot <- renderPlotly(data$event_consolidation %>% event_impact_chart() %>% maybe_show_event(data$event))
    })

    output$event_report <- downloadHandler(
      filename = function() {
        ext <-switch(
          input$format, PDF = '.pdf', HTML = '.html', Word = '.docx'
        )
        owner_name = owner_name()
        paste("Event_Report_", owner_name, "_", gsub("-", "_", Sys.Date()), ext, sep="") # For PDF output, change this to "report.pdf"
      },
      content = function(file) {
        print(paste("Content file", file))
        e <- req(e())
        tempReport <- file.path(tempdir(), "event_report.Rmd")
        file.copy("event_report.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          event_start= e$start_date,
          set_title =  paste("Event report", e$name,"for", req(owner_name())),
          owner_name = owner_name(),
          ae_dist_chart = ae_dist_chart(),
          a_vs_e_chart =a_vs_e_chart(),
          meter_events = meter_events(),
          intermediary_mention = intermediary_mention(),
          ouehp = ap$ouehp() %>% maybe_show_event(e),
          ouhp = ap$ouhp() %>% maybe_show_event(e),
          oucp = ap$oucp() %>% maybe_show_event(e)
        )

        out <- rmarkdown::render(
          tempReport,
          output_format = switch(input$format, PDF = pdf_document(), HTML = html_document(), Word = word_document()),
          output_file = file, params = params, envir = new.env(parent = globalenv()))
        file.rename(out, file)
      }
    )
  })


}
