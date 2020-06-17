

#' @export
dbq <- function(sql, ...){
  DBI::dbGetQuery(pool, statement = DBI::sqlInterpolate(.GlobalEnv$pool, sql, ... ))
}


meter_detail <- "SELECT o.id as oid, o.owner, pm.mid::text, pm.mpr, poi_id, pm.utility, pm.activity ,pm.intermediary_id, i.intermediary, pm.n , pm.nnz, pm.recent_zero,
                               pm.latest_trend, pm.latest_duration, pm.prior_trend, pm.prior_duration, pm.big_trend, pm.big_duration, pm.summer_quartile, pm.winter_quartile,
                               pm.convexity, pm.spearman, pm.madness, pm.power_to FROM v1.poi_meters pm, v1.poi poi, v1.owner o, v1.intermediary i
                               WHERE poi.id = pm.poi_id AND o.id = poi.owner_id"
#' @export
read_meter_detail_by_mpr <- function(mpr){
  print(paste("Reading MPR Detail", mpr))
  dbq(paste(meter_detail,  "AND pm.mpr = ?mpr and i.id = pm.intermediary_id;"), mpr = mpr)
}

#' @export
read_meter_detail_by_mid <- function(mid){
  print(paste("Reading MID Detail", mid))
  dbq(paste(meter_detail, "AND pm.mid = ?mid and i.id = pm.intermediary_id;"), mid = as.numeric(mid))
}

#' @export
locations <- function()
  dbq("SELECT *  FROM weather.location")


#' @export
owner_deferrals <- function(oid)
  dbq("SELECT oid::text as oid, mid::text as mid, until FROM v1.defer WHERE oid = ?oid", oid = oid)

#' @export
owner_meter_user_states <- function(oid)
  dbq("SELECT oid::text as oid, mid::text as mid, user_state FROM v1.defer WHERE oid = ?oid", oid = oid)

#' @export
upsert_owner_deferral <- function(oid, mid, until)
  dbq("INSERT INTO v1.defer(oid, mid, until) VALUES(?oid, ?mid, ?until)  ON CONFLICT defer_reference_pkey DO UPDATE SET until = ?until;", oid = as.numeric(oid), mid = as.numeric(mid), until = until)

#' @export
upsert_meter_user_states <- function(oid, mid, user_state)
  dbq("INSERT INTO v1.defer(oid, mid, user_state) VALUES(?oid, ?mid, ?user_state)  ON CONFLICT mus_reference_pkey DO UPDATE SET user_state = ?user_state;", oid = as.numeric(oid), mid = as.numeric(mid), until = until)


#' @export
meter_days_for_owner_utility <- function(oid, utility)
  dbq("SELECT mid::text as mid, mpr, unnest(ts) as ts, unnest(oat)/1000 as oat, unnest(watts)/1000 as kw, unnest(fit)/1000 as expected, unnest(model) as model FROM v1.poi_meters pm, v1.poi p WHERE pm.poi_id = p.id AND p.owner_id = ?oid and pm.utility = ?utility;", oid = oid, utility = utility)

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
meter_statistics <- function(oid, utility){
  dbq("SELECT pm.mid::text, pm.mpr, pm.activity, pm.update_status, update_status_prior, update_status_changed, pm.n, pm.nnz, pm.recent_zero, pm.power_to,
  pm.summer_quartile, pm.winter_quartile, pm.convexity, pm.spearman, pm.madness,
  pm.latest_trend, pm.latest_duration, pm.prior_trend, pm.prior_duration, pm.big_trend, pm.big_duration,
  poi_id, poi.longitude, poi.latitude, o.id as oid, o.owner, pm.utility, pm.intermediary_id  FROM v1.poi_meters pm, v1.poi poi, v1.owner o
  WHERE poi.id = pm.poi_id AND o.id = poi.owner_id AND utility = ?utility  AND o.id = ?oid;", utility = utility, oid = oid)
}


#' @export
read_owner_events_by_owner <- function(oid)
  dbq("SELECT oe.* FROM v1.owner_event oe WHERE oe.owner_id = ?oid ORDER BY owner_event DESC;", oid = oid)


profile_selection <- "SELECT id::text as mid, ts, power, h0000, h0030, h0100, h0130, h0200, h0230, h0300, h0330, h0400, h0430, h0500, h0530, h0600, h0630, h0700, h0730, h0800, h0830, h0900, h0930, h1000, h1030, h1100, h1130, h1200, h1230, h1300, h1330, h1400, h1430, h1500, h1530, h1600, h1630, h1700, h1730, h1800, h1830, h1900, h1930, h2000, h2030, h2100, h2130, h2200, h2230, h2300, h2330 FROM prod.meter_day_48 WHERE id = ?mid"

#' @export
day_profile <- function(mid, ts)
  dbq(paste(profile_selection, "AND ts = ?ts;"), mid = as.numeric(mid), ts = ts)


prop <- function(v, p)
  if_else(p > 0, v/p, 0)

#' @export
meter_pentile_profiles <- function(mid){
  pentiles <- dbq( "SELECT * FROM  v1.pentile_profiles WHERE mid = ?mid;", mid = as.numeric(mid))
  rph = round((ncol(pentiles)-2)/24)
  mutate_at(pentiles, vars(starts_with("h")), ~prop(. * rph, power))
}

#
# sql = paste("WITH m AS (SELECT poi_id as poid, update_status, mid::text as mid, utility, intermediary_id as iid, trim(mpr::text) as mpr FROM v1.poi_meters),",
#              "p AS (SELECT id as poid, owner_id as oid, postcode, location_id::text, latitude, longitude FROM v1.poi),",
#              "o AS (SELECT id as oid, owner, match_ref FROM v1.owner),",
#              "i AS (SELECT id as iid, intermediary FROM v1.intermediary)",
#              "SELECT m.*, p.postcode, p.location_id, p.latitude, p.longitude, i.intermediary, o.oid, o.owner, o.match_ref FROM m, p, o, i",
#              "WHERE  m.iid = i.iid AND m.poid = p.poid AND o.oid = p.oid AND o.oid = ?oid and m.utility = ?utility")
#
# meter_meta <- dbq(sql, oid = oid, utility = utility)
