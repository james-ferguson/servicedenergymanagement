nl_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NL',
    h2("No Location")

  )

}

nl_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
