

#' @export
pf_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'PF',
    h2("Meters reading current zeroes when this is considered suspicious"),
    table_meter_selection_ui(ns('meters'))
  )

}


#' @export
pf_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
