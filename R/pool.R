
#' @export
get_pool <- function(){
  options(scipen = 12)
  conn_args <- config::get("dataconnection")
  pool::dbPool(odbc::odbc(), Driver = conn_args$driver, Server = conn_args$server, UID = conn_args$uid, PWD = conn_args$pwd,
                       Port = conn_args$port, Database = conn_args$database, BoolsAsChar = 0, MaxVarcharSize = 0)
}
