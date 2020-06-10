kpi_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'KPI',
    h2("Comparative KPI")

  )

}

kpi_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
