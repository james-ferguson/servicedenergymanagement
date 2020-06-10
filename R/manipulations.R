
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
    mutate( recent_zero = count_recent_zero(data), last_reading = lr(data))
}

summarise_meter_days <- function(owner_utility_meter_days){
  rz <- calculate_recent_zeroes_by_mid(df=owner_utility_meter_days)

  owner_utility_meter_days %>%
    group_by(mid) %>%
    arrange(desc(ts)) %>%
    summarise(mpr = first(mpr),
              expected = mean(expected, na.rm = TRUE),
              actual = mean(kw, na.rm = TRUE), .groups = 'drop') %>%
    mutate(turndown = round(100 * actual / expected,1)) %>%
    left_join(rz, by = "mid")
}
