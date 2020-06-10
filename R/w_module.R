
w_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'W',
    h2("Excessive")

  )

}

w_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
