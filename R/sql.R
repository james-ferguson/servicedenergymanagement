

#' @export
dbq <- function(sql, ...){
  dbGetQuery(pool, statement = DBI::sqlInterpolate(.GlobalEnv$pool, sql, ... ))
}

#' @export
meter_days_for_owner_utility <- function(oid, utility)
  dbq("SELECT mid::text as mid, mpr, unnest(ts) as ts, unnest(oat)/1000 as oat, unnest(watts)/1000 as kw, unnest(fit)/1000 as expected FROM v1.poi_meters pm, v1.poi p WHERE pm.poi_id = p.id AND p.owner_id = ?oid and pm.utility = ?utility;", oid = oid, utility = utility)

#' @export
read_owner_utility_consolidation <- function(owner_id, utility){
  df <- dbq("SELECT owner_id, utility, ts, oat, kw, fit, e, abs_e, p_e, n_e FROM v1.owner_utility WHERE owner_id = ?owner_id AND utility = ?utility;", owner_id = owner_id, utility = utility)
  df %>%
    mutate(dow = factor(
      weekdays(ts),
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ))
}


#' @export
read_owner_utility_budget_consolidation <- function(owner_id, utility){
  df <- dbq("SELECT owner_id, utility, budget_start, budget, ts FROM v1.owner_utility_budget WHERE owner_id = ?owner_id AND utility = ?utility;", owner_id = owner_id, utility = utility)
  df %>%
    mutate(dow = factor(
      weekdays(ts),
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ))
}

#' @export
meter_statistics <- function(oid, utility, pool){

  if(oid == 0){
    oid_spec = ";"
    dots =list(utility = utility)
  } else {
    oid_spec = " AND o.id = ?oid;"
    dots =list(utility = utility, oid = oid)
  }

  stats_meter_data <- dbq(paste0("SELECT pm.mid::text, pm.mpr, pm.activity, pm.update_status, update_status_prior, update_status_changed, pm.n, pm.nnz, pm.recent_zero, pm.power_to,
  pm.summer_quartile, pm.winter_quartile, pm.convexity, pm.spearman, pm.madness,
  pm.latest_trend, pm.latest_duration, pm.prior_trend, pm.prior_duration, pm.big_trend, pm.big_duration,
  poi_id, poi.longitude, poi.latitude, o.id as oid, o.owner, pm.utility, pm.intermediary_id  FROM v1.poi_meters pm, v1.poi poi, v1.owner o
  WHERE poi.id = pm.poi_id AND o.id = poi.owner_id AND utility = ?utility ", oid_spec), .dots = dots)
}


#' @export
read_owner_events_by_owner <- function(oid)
  dbq("SELECT oe.* FROM v1.owner_event oe WHERE oe.owner_id = ?oid ORDER BY owner_event DESC;", oid = oid)


#
# sql = paste("WITH m AS (SELECT poi_id as poid, update_status, mid::text as mid, utility, intermediary_id as iid, trim(mpr::text) as mpr FROM v1.poi_meters),",
#              "p AS (SELECT id as poid, owner_id as oid, postcode, location_id::text, latitude, longitude FROM v1.poi),",
#              "o AS (SELECT id as oid, owner, match_ref FROM v1.owner),",
#              "i AS (SELECT id as iid, intermediary FROM v1.intermediary)",
#              "SELECT m.*, p.postcode, p.location_id, p.latitude, p.longitude, i.intermediary, o.oid, o.owner, o.match_ref FROM m, p, o, i",
#              "WHERE  m.iid = i.iid AND m.poid = p.poid AND o.oid = p.oid AND o.oid = ?oid and m.utility = ?utility")
#
# meter_meta <- dbq(sql, oid = oid, utility = utility)
