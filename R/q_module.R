
#' @export
q_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'Q',
    h2("Meters where the current state could not be calculated"),
    table_meter_selection_ui(ns('meters'))
  )

}

#' @export
q_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
