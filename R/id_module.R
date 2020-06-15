

#' @export
id_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'ID',
    h2("Meters with insufficient data to model performance"),
    table_meter_selection_ui(ns('meters'))
  )

}


#' @export
id_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}

