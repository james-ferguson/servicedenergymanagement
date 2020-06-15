

owner_table <- function(data){
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
owner_ui <- function(id, width = "100%", height = "auto"){
  ns <- NS(id)
  DT::dataTableOutput(ns("table"), width = "100%", height = height)
}

#' @export
owner_server <- function(id, data){
  moduleServer(id, function(input, output, session) {

    session$userData$owner_name = reactiveVal()
    session$userData$oid = reactiveVal()

    df_table <- reactive({
      df <- req(data())
      owner_table(df)
    })

    output$table <-  DT::renderDataTable(req(df_table()))

    observeEvent(input$table_rows_selected,{
      df <- data()
      js$collapse("owner_selection_box")
      owner <- df[input$table_rows_selected,]
      session$userData$owner_name(owner$owner)
      session$userData$oid(owner$oid)
    })

    observe({
      print(session$userData$oid())
    }, priority = 11)

    session$userData$oid

  })
}
