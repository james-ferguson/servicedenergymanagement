nw_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NW',
    h2("No Weather")

  )

}

nw_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
