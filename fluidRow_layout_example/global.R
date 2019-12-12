library(DBI)



get_pg_connection <- function(user_name, pwd) {
  pg_conn <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    dbname = 'metis',
    host = 'localhost',
    port = 1234,
    user = user_name,
    password = pwd
  )
  return(pg_conn)
}

close_pg_connection <- function(pg_conn) {
  DBI::dbDisconnect(pg_conn)
}

close_all_tabs <- function() {
  hide(selector = '#tabs li a[data-value="blood_rcpt_storage_form"]')
  hide(selector = '#tabs li a[data-value="manuf_sample_rcpt_proc_storage_form"]')
  hide(selector = '#tabs li a[data-value="tissue_sample_receipt_storage_form"]')
}

get_user_list <- function() {
  metis_app_name <- 'METIS Translational Science - Input Forms'
  sqlite_conn <- DBI::dbConnect(RSQLite::SQLite(), '../metis_admin/user_lists.sqlite')
  sql_tmpl <- "SELECT user_name FROM user_access WHERE metis_app_name = ?metis_app_name ORDER BY 1"
  sql <- DBI::sqlInterpolate(DBI::ANSI(), sql_tmpl, metis_app_name = metis_app_name)
  result <- DBI::dbGetQuery(sqlite_conn, sql)
  DBI::dbDisconnect(sqlite_conn)
  return(as.vector(result$user_name))
}

get_study_site_names <- function(pg_conn) {
  sql <- "SELECT * FROM shiny_ui.get_study_site_names()"
  study_site_names <- DBI::dbGetQuery(pg_conn, sql)
  return(as.vector(study_site_names$study_site_name))
}
get_study_codes <- function(pg_conn) {
  sql <- "SELECT * FROM shiny_ui.get_study_codes()"
  study_codes <- DBI::dbGetQuery(pg_conn, sql)
  return(as.vector(study_codes$study_code))
}
get_patient_uins_site_study <- function(pg_conn, site_name, study_code) {
  sql_tmpl <- "SELECT * FROM shiny_ui.get_patient_uins_site_study(?site_name, ?study_code)"
  sql <- sqlInterpolate(DBI::ANSI(), sql_tmpl, site_name = site_name, study_code = study_code)
  patient_uins <- DBI::dbGetQuery(pg_conn, sql)
  return(as.vector(patient_uins$patient_uin))
}
