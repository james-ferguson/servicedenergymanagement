nc_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NC',
    h2("Non-Comms.")

  )

}

nc_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
