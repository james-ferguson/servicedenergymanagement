
nt_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NT',
    h2("No Trend")

  )

}

nt_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
