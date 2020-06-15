
#' @export
np_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NP',
    h2("Meters where AMR has never recorded a positive reading"),
    table_meter_selection_ui(ns('meters'))
  )

}


#' @export
np_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}

