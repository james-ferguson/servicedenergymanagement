

#' @export
nt_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NT',
    h2("Meters On Target"),
    table_meter_selection_ui(ns('meters'))
  )

}


#' @export
nt_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}

