

single_selection_data_table <- function(data){
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
single_row_selector_ui <- function(id, width = "100%", height = "auto"){
  ns <- NS(id)
  DT::dataTableOutput(ns("table"), width = "100%", height = height)
}

#' @export
single_row_selector_server <- function(id, data){
  moduleServer(id, function(input, output, session) {
    df_table <- reactive({
      df <- req(data())
      single_selection_data_table(df)
    })
    output$table <-  DT::renderDataTable(req(df_table()))
    eventReactive(input$table_rows_selected,{
      df <- data()
      df[input$table_rows_selected,]
    })
  })
}
