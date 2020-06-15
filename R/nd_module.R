#' @export
nd_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'ND',
    h2("Meters with No AMR Data"),
    table_meter_selection_ui(ns('meters'))
  )

}

#' @export
nd_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
