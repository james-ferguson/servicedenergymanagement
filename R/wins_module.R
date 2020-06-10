wins_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'WINS',
    h2("Success")

  )

}

wins_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
