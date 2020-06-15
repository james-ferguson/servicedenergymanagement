nl_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'NL',
    h2("Meters to locate"),
    em("Meter:"),
    verbatimTextOutput(ns("no_location_meter"), placeholder = TRUE),
    em("Location:"),
    verbatimTextOutput(ns("map_location"), placeholder = TRUE),

    leafletOutput(ns("mymap"),height =620),
    actionBttn(ns("locate"), "Confirm Location",  icon = icon("map-signs"), color = "success"),

    helpText( "Please select one of the meters below and determine where it is on the 'Locations' map by clicking - location which will be shown below and marked on the map. If you are happy with the location for that meter, click confirm location and the meter location will be stored, a source of weather data arranged and assuming AMR collection functionaliy meter diagnosis will commence tonight."),
              table_meter_selection_ui(ns('meters'))
  )

}

nl_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    table_meter_selection_server('meters', df)

    observeEvent(session$userData$selected_meter(), {
      m <- req(session$userData$selected_meter())
      map_click()
      output$no_location_meter <- renderText( paste(
        "Meter reference:", m$mpr,
        "\nGlobal ref:", m$mid,
        "\nActivity:", m$active,
        "\nLast Update Status:", m$update_status,
        "\nLast Changed:", m$update_status_changed,
        "\nOwned / Operated by:", m$owner
      ) )
    })

    map_click <- reactiveVal()

    observeEvent(input$mymap_click, {
      p <- input$mymap_click
      map_click(p)
      output$map_location <- renderText({ paste("Chosen Meter Position:\nLongitude:", p$lng, "\nLatitude:", p$lat, ".") })
      isolate({
        map <- leafletProxy('mymap') %>%
          clearGroup('points')

        addMarkers(map, lng = p$lng, lat = p$lat, group = 'points')
      })

    })



    weather_locations <- reactive(events::locations(pool) %>% select(id, lng=longitude, lat=latitude, postcode) %>% na.omit())


    observeEvent(input$locate, {

      p <- map_click()

      if(is.null(p)){
        modalDialog(icon("ban"), "Sorry we cannot locate the meter until you have chosen a map point", title = "Map location not found", footer = modalButton("Dismiss"),
                    size = "s", easyClose = TRUE, fade = TRUE) %>% showModal()
        return()
      }
      locs <- req(weather_locations())

      locs$dist = (p$lng -locs$lng)^2 + (p$lat - locs$lat)^2
      closest = min(locs$dist, na.rm = TRUE)
      weather <- locs[match(closest, locs$dist),]
      pc = ""

      pc = if(!is.na(weather$postcode))
        pc = p(paste("The nearest postcode (to the weather station) is",weather$postcode))

      modalDialog(title = span(icon("sun"), "Weather source found !"), p("Great, we found a nearby reliable source of weather data and forecasts..."),
                  p(paste("Weather Station id:", weather$id)),
                  p(paste("Longitude:", weather$lng)),
                  p(paste( "Latitude:", weather$lat)) , pc, footer = modalButton("Success"),
                  size = "m", easyClose = TRUE, fade = TRUE) %>% showModal()

      isolate({
        leafletProxy('mymap') %>%
          addMarkers( lng = weather$lng, lat = weather$lat, group = 'points') %>%
          flyToBounds(lng1 = max(p$lng, weather$lng),lat1 = max(p$lat, weather$lat),
                      lng2 = min(p$lng, weather$lng),lat2 = min(p$lat, weather$lat))
      })

    })


    output$mymap <- renderLeaflet({#names(providers)
      points = tibble::tibble(longitude = c(-6.470947,2.010498), latitude = c(50.45401, 59.51203))
      leaflet(data = points) %>%
        # addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(noWrap = TRUE)) %>%

        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
        # addProviderTiles(providers$Stamen.Watercolor, options = providerTileOptions( opaciy = 0.1, noWrap = TRUE)) %>%
        flyToBounds(lng1 = max(points$longitude),lat1 = max(points$latitude),
                    lng2 = min(points$longitude),lat2 = min(points$latitude))  %>%   addSearchOSM(options = searchOptions(collapsed = FALSE))
    })


  })
}

#
