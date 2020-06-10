s_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'S',
    h2("Improved")

  )

}

s_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
