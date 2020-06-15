
#' @export
nw_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NW',
    h2("Meters where a weather source has been identified but weather data is missing"),
    table_meter_selection_ui(ns('meters'))
  )

}


#' @export
nw_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
