

meter_table <- function(data){
  DT::datatable(
    data,
    options = list( dom = "Bftipr",
                    buttons = c(I("colvis"), I("copy"), I("csv"), I("excel"), I("pdf")),
                    autoWidth = TRUE,
                    pageLength = 12
    ),
    class = "display",
    #callback = JS("return table;"),
    rownames = FALSE,
    filter = "top",
    escape = TRUE,
    style = 'default',
    width = NULL,
    height = NULL,
    elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", TRUE),
    selection = "single",
    extensions = list(),
    plugins = NULL,
    editable = FALSE
  )
}

#' @export
table_meter_selection_ui <- function(id, width = "100%", height = "auto"){
  ns <- NS(id)
  DT::dataTableOutput(ns("table"), width = "100%", height = height)
}

#' @export
table_meter_selection_server <- function(id, meters){

  moduleServer(id, function(input, output, session) {
    output$table <-  DT::renderDataTable({
      m <- req(meters())
      meter_table(m)
    })

    observeEvent(input$table_rows_selected,{
      df <- meters()
      m <- df[input$table_rows_selected,]
      session$userData$selected_meter(m)
    })

    session$userData$selected_meter

  })
}
