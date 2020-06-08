

#' @export
q <- function(sql, ...){
  dbGetQuery(pool, statement = DBI::sqlInterpolate(.GlobalEnv$pool, sql, ... ))
}
