wins_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'WINS',
    h2("Success"),
    table_meter_selection_ui(ns('meters'))
  )

}

wins_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
