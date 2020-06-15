
#' @export
dow_target_profile <- function(day_pentiles, selected_day){

  selected_day <- selected_day[1,]
  print(selected_day)

  day_oat = selected_day$oat*100
  day_fit = selected_day$expected

  index <- as.integer(sum(day_oat >  day_pentiles$oat))
  b_index <- if_else(index == 0L, 1L, index)
  a_index <- if_else(b_index == 5L, 5L, b_index+1L)
  p_below <- day_pentiles[b_index,]
  p_above <- day_pentiles[a_index,]

  oat_spread = p_above$oat - p_below$oat

  my_table <- tibble(oat = day_oat, index, b_index, a_index) %>%
    mutate(above_lower = oat - p_below$oat,
           raw_lower_prop = if_else(above_lower > 0 , above_lower / oat_spread, 1 ),
           lower_prop = if_else(is.infinite(raw_lower_prop),0.5,raw_lower_prop) ) %>%
    select(lower_prop)

  p_below$w = my_table$lower_prop
  p_above$w = 1-p_below$w

  f <- bind_rows(
    mutate_at(p_below, vars(starts_with("h")), ~`*`(., w*day_fit)) %>% select(starts_with("h")) ,
    mutate_at(p_above, vars(starts_with("h")), ~`*`(., w*day_fit)) %>% select(starts_with("h")))

  cbind(selected_day, t(colSums(f)))
}

#' @export
filter_meter_events <- function(df, event){
  event_start = event$start_date
  event_end = ifelse(is.na(event$end_date), Sys.Date(), event$end_date)
  df %>%
    filter(ts >= event_start, ts <= event_end)
}

#' @export
consolidate_owner_utility_meter_days <- function(owner_utility_meter_days)
  owner_utility_meter_days %>% select(-mid, -mpr) %>%
  mutate(waste = kw - expected, abs_e = abs(waste), p_e = waste * (waste > 0), n_e  = waste * (waste < 0), n = 1) %>% group_by( ts, .drop = ts) %>%
  summarise_if(is.numeric, sum, na.rm=TRUE)

#' @export
filter_history_by_period <- function(df, period){
  today <- max(df$ts)
  ys <- as.Date(paste(format(Sys.Date(), "%Y"), "01-01", sep = "-"))
  rms <- floor_date(today - months(1), "month")
  rmq <- floor_date(today - months(3), "month")
  tr <- switch(period,
               "1"={c(ys  , today)}, # YTD
               "2"={c(today - 366  , today)}, # RY
               "3"={c(rmq  , today)}, # RQ
               "4"={c(rms  , today)}, # RM
               "5"={c(today - 6  , today)}, # RW
               "6"={c(as.Date("2000-01-01")  , today)}) # ALL

  df %>% filter(ts >= tr[1], ts <= tr[2])
}

count_recent_zero <- function(df){
  dfkw <- arrange(df, desc(ts)) %>% select(kw, ts) %>% na.omit()
  recent_zero = sum(cumsum(dfkw$kw)==0)
  last_non_zero = dfkw[recent_zero+1,]$ts
}

lr <- function(df){
  max(df$ts)
}

calculate_recent_zeroes_by_mid <- function(df){
  nest_by(df, mid) %>%
    mutate( recent_zero = count_recent_zero(data), last_reading = lr(data)) %>% select(-data)
}

summarise_meter_days <- function(owner_utility_meter_days){
  rz <- calculate_recent_zeroes_by_mid(df=owner_utility_meter_days)

  owner_utility_meter_days %>%
    group_by(mid) %>%
    arrange(desc(ts)) %>%
    summarise(mpr = first(mpr),
              expected = mean(expected, na.rm = TRUE),
              actual = mean(kw, na.rm = TRUE), .groups = 'drop') %>%
    mutate(turndown = round(100 * actual / expected,1)) #%>%
   # left_join(rz, by = "mid")
}
