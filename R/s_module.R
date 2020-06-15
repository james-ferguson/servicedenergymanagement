s_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'S',
    h2("Improved"),
    table_meter_selection_ui(ns('meters'))
  )

}

s_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}

