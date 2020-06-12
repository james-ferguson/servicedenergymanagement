
w_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'W',
    h2("Excessive"),
    table_meter_selection_ui(ns('w_meters'))
  )

}

w_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
    meters <- table_meter_selection_server('w_meters', 'W')
  })
}
