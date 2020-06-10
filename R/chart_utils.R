#  palette.pals() ] "R3"   "R4"    "ggplot2" "Okabe-Ito"  "Accent" "Dark 2"  "Paired"  "Pastel 1"
# "Pastel 2" "Set 1"  "Set 2"  "Set 3"] "Tableau 10""Classic Tableau" "Polychrome 36"   "Alphabet"

cols <- palette.colors(8, "R4")

options("lubridate.week.start" = 1)

f <- list(
  size = 16,
  family = 'sans-serif'
)
m <- list(
  l = 100,
  r = 50,
  b = 0,
  t = 0,
  pad = 4
)

#' @import plotly
#' @export
chart <- function(data = NULL, ...){
  if(!is.null(data))
    p <- plot_ly(data, colors = cols,...)
  else
    p <-  plot_ly(colors = cols,...)
  config(p,  displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "lasso2d", "select2d", "pan2d", "zoomIn2d", "zoomOut2d")) %>%
   plotly::layout(font = f, margin = m)
}

#' @export
maybe_show_event <- function(p, event){

  if(!is.null(event)){
    if(nrow(event) >0){

      if(is.na(event$end_date))
        event$end_date = Sys.Date()

      rect1 <- list(
        type = "rect",
        xref = 'x',
        yref = "paper",
        fillcolor = "rgba(62, 221, 128, 0.2)",
        line = list(color = 'rgb(62, 221, 128)', size=2),
        x0 = event$start_date,
        x1 = event$end_date,
        y0 = 0,
        y1 = 1
      )

      p <- p %>%
        plotly::layout(
          shapes = rect1
        )
    }}
  p

}


#' @export
ae_dist_chart <- function(me){

  me$turndown[me$turndown > 200] <- 200

  p <- chart(x = ~me$turndown, type = "histogram", nbinsx = 40)  %>%
    plotly::layout(
      yaxis = list(title = 'Meters with Turndown ', rangemode = "tozero"),
      xaxis = list(title = 'Turndown %', range = c(0,200))
    )
}

#' @export
a_vs_e_chart <- function(me, s){ # s is selected meter

  mm=pmax(max(me$expected,na.rm=TRUE), max(me$actual,na.rm=TRUE))

  p <-
    me %>% chart(source = "meter_selection_chart") %>%
    add_polygons(x=c(0,mm,mm),y=c(0,mm,0), color = I("green"),fillcolor = 'rgba(7, 164, 7, 0.2)', name = "Reduction") %>%
    add_lines(x = c(0,mm), y = c(0, mm*0.75),color = I("gray75"), name = "Turndown to 75%") %>%
    add_lines(x = c(0,mm), y = c(0, mm*0.50),color = I("gray50"), name = "Turndown to 50%") %>%
    add_lines(x = c(0,mm), y = c(0, mm*0.25),color = I("gray25"), name = "Turndown to 25%") %>%
    add_markers(
      x =  ~ expected,
      y = ~ actual,
      text = ~ paste("MPR:", mpr, "<br>", turndown, "%"),
      key =  ~ mid,
      name = "Meters",
      marker = list(color = 'rgba(125,0,255, 0.9)',size = 5)
    )  %>%
    plotly::layout(
      yaxis = list(title = 'Actual kWh', range = c(0,mm)),
      xaxis = list(title = 'Normally Expected kWh', range = c(0,mm))
    )
  if(!is.null(s) && nrow(s)==1){
    sme <- me[me$mid == s$mid,]
    if (!is.null(sme)){
      p <-
        p %>% add_markers(
          data = sme,
          x =  ~ expected,
          y = ~ actual,
          text = ~ mid,
          color = I("cyan"),
          name = "Selected",
          marker = list(color = I("red"),size = 8))
    }
  }
  p
}


#' @export
event_context_impact_chart <- function(df){
  df %>%
    chart() %>%
    add_markers(x=~ts, y = ~kw/1000, color=~dow, text = ~ts, legendgroup="group1" )  %>%
    add_annotations(x = ~ts,
                    y = ~kw/1000,
                    text = "",
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowwidth = 0.8,
                    arrowcolor=~dow,
                    arrowhead = 1.8,
                    arrowsize = 1,
                    standoff = 5,
                    ax = ~ts,
                    ay = ~expected/1000,
                    axref="x",
                    ayref='y') %>%
    plotly::layout(
      yaxis = list(title = 'Megawatts', rangemode = "tozero"),
      xaxis = list(title = '')
    )
}


#' @export
event_impact_chart <- function(ou_time_selected){
  ou_time_selected %>%
    chart() %>%
    add_markers(x=~ts, y = ~kw/1000, color=~dow, text = ~ts, legendgroup="group1" )  %>%
    add_annotations(x = ~ts,
                    y = ~kw/1000,
                    text = "",
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowwidth = 0.8,
                    arrowcolor=~dow,
                    arrowhead = 1.8,
                    arrowsize = 1,
                    standoff = 5,
                    ax = ~ts,
                    ay = ~expected/1000,
                    axref="x",
                    ayref='y') %>%
    plotly::layout(
      yaxis = list(title = 'Megawatts', rangemode = "tozero"),
      xaxis = list(title = '')
    )
}

#' @export
selected_period_oat_chart <- function(ou_consolidation, ou_time_selected){
  all <- ou_consolidation
  s <- ou_time_selected

  yax <- 'kiloWatts)'

  if(max(all$kw,na.rm=TRUE) > 10000){
    yax <- 'Megawatts'
    s <- s %>% mutate(kw = round(kw/1000,1))
    all <- all %>% mutate(kw = round(kw/1000,1))
  } else{
    s <- s %>% mutate(kw = round(kw,1))
    all <- all %>% mutate(kw = round(kw,1))
  }

  chart() %>%
    add_markers(data=all, x=~round(oat/10,2), y = ~kw, marker=list(color= I("lightgray"), size = "2.75"), name="Other Periods", text = ~ts) %>%
    add_markers(data = s, x=~round(oat/10,2), y = ~kw, color=~dow, text = ~ts) %>%


    plotly::layout(
      yaxis = list(title = yax, rangemode = "tozero"),
      xaxis = list(title = 'Outside Air °C')
    )
}

#' @export
high_low_forecast_errors <- function(ou_time_selected){
  mxts = max(ou_time_selected$ts)
  mnts = min(ou_time_selected$ts)
  ou_time_selected %>%
    arrange(ts) %>%
    chart() %>%
    add_lines(x=~ts, y = ~p_e/1000, name = "∑ High Errors") %>%
    add_lines(x=~ts, y = ~n_e/1000, name = "∑ Low Errors") %>%
    add_lines(x=~ts, y = ~waste/1000, name = "∑ Forecast Errors") %>%
    plotly::layout(
      yaxis = list(title = 'Forecast Error Power (MW)', rangemode = "tozero"),
      xaxis = list(title = '')
    )

}

cs <- function(df, key){
  df %>% arrange(ts) %>% mutate(cse = 24*cumsum(waste)/1000)
}

#' @export
cumulative_waste_impact_chart <- function(ou_time_selected){

  ou_time_selected %>%
    group_by(dow) %>%
    group_modify(~cs(.x, .y)) %>%
    chart() %>%
    add_lines(x=~ts, y = ~cse, color=~dow) %>%
    plotly::layout(
      yaxis = list(title = 'Cumulative Impact (MWh)', rangemode = "tozero"),
      xaxis = list(title = '')
    )

}



