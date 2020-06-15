nc_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NC',
    h2("Non. Comms"),
    table_meter_selection_ui(ns('meters'))
  )

}

nc_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
