owner_events_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'EVENTS',
    h2("Events"),
    p('Please choose existing event or define new requirements'),
    fluidRow(
      column(width = 6,
             h3('Existing Event'),
             event_choice_UI(ns('ecm')),
             hr(),
             helpText("A 'new normal' event affects expected consumption as the system 'learns', thus the evental impact of a 'new-normal' event tends to zero.
    Otherwise the event is treated as exceptional  so experiences during the event are excluded from learning algoriths. The assumption is that perfromance will return to prior levels at the end of the event.
    A new event whose dates overlap any previously defined event takes precedence (updates cease for the old event).")
      ),
      column(width = 6,
             h3('New Event'),
             event_creator_UI(ns('evc'))
      )
    ),
    fluidRow(
      box(
        title = "Actual vs. Expected",
        width = 3,
        plotly::plotlyOutput(ns("a_vs_e_plot"), height = 400)
      ),
      box(
        title = "Distribution of Turndown/Excess",
        width = 3,
        plotly::plotlyOutput(ns("ae_dist_plot"), height = 400)
      ),
      tabBox(
        title = tagList(shiny::icon("chart"), ""),
        width =6,
        tabPanel("Consumption History", plotly::plotlyOutput(ns("owner_utility_history_plot"), height = 400), value = "Consumption Chart"),
        tabPanel("Forecast Accuracy", plotly::plotlyOutput(ns("owner_utility_error_history_plot"), height = 400), value = "Error Chart"),
        tabPanel("Cumulative Deviation", plotly::plotlyOutput(ns("owner_utility_cusum_plot"), height = 400), value = "Era Chart")
      )
    ),
    fluidRow(
    column(width = 3, offset = 6,
           radioButtons(ns('format'), 'Document format (currently only html)', c('HTML', 'Word','PDF'), inline = TRUE)),
           column(width = 3, downloadButton(ns("event_report"), "Download report"))
    ),
    single_row_selector_ui(ns("meter_events"))
  )

}

owner_events_server <- function(id, owner, utility, ap){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
    event_data <- reactiveVal()

    chosen_event_data <- event_choice_server('ecm', owner, utility)
    created_event_data <- event_creator_server('evc', owner, utility)
    observeEvent(chosen_event_data(), event_data(chosen_event_data()) )
    observeEvent(created_event_data(), event_data(created_event_data()) )


    observeEvent(event_data(),{
      data <- req(event_data())
      output$a_vs_e_plot <- renderPlotly(data$a_vs_e_plot)
      output$ae_dist_plot <- renderPlotly(data$ae_dist_plot)
      output$owner_utility_error_history_plot <- renderPlotly(data$owner_utility_error_history_plot)
      output$owner_utility_cusum_plot <- renderPlotly(data$owner_utility_cusum_plot)
      output$owner_utility_history_plot <- renderPlotly(data$owner_utility_history_plot)

    })

    output$event_report <- downloadHandler(
      filename = function() {
        'index.html'
      },
      content = function(file) {
        params <- req(event_data())
        print(names(params))
        params$title = paste("Event report", params$event$name, "<br>for", params$owner$owner)
        print(paste("Content file", file))
        tempReport <- file.path(tempdir(), "generic_event_report.Rmd")

        src = system.file("generic_event_report.Rmd", package = "servicedenergymanagement", lib.loc = NULL, mustWork = TRUE)
        file.copy(src, tempReport, overwrite = TRUE)
        out <- rmarkdown::render(
          tempReport,
          output_format = rmarkdown::html_document(),
          output_file = file, params = params, envir = new.env(parent = globalenv()))
        file.rename(out, file)
      }
    )
  })


}
