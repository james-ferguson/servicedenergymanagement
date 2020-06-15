kpi_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'KPI',
    h2("Comparative KPI Dialogues"),
    table_meter_selection_ui(ns('meters'))
  )

}

kpi_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
