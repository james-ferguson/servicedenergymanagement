s_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'S',
    h2("Saving"),
    table_meter_selection_ui(ns('s_meters'))
  )

}

s_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
    meters <- table_meter_selection_server('s_meters', 'S')
  })
}
