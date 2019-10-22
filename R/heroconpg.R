#' Create arguments for the heroku CLI
#'
#' @param prop Property to look for
#' @param app Target heroku app
#'
#' @return
#' @export
#'
#' @examples
create_heroku_pgurl_args <- function(prop, app) {
  c("config:get", prop, "-a", app)
}


#' Run command on UNIX os
#'
#' @param heroku_cli Path of the Heroku CLI
#' @param arguments Vector of arguments for CLI
#'
#' @return
#' @export
#'
#' @examples
run_unix_cmd <- function(heroku_cli = "/snap/bin/heroku",
                         arguments) {
  processx::run(heroku_cli, arguments)
}


#' Run command on LINUX os
#'
#' @param heroku_cli Path of the Heroku CLI
#' @param arguments Vector of arguments for CLI
#'
#' @return
#' @export
#'
#' @examples
run_linux_cmd <- function(heroku_cli = "heroku",
                          arguments) {
  paste(c(heroku_cli, arguments), collapse = " ") %>%
    base::system(intern = TRUE)
}


#' Get postgres db url from Heroku
#'
#' @param prop Property to look for
#' @param app Target Heroku app
#' @param os Target Operating System
#'
#' @return
#' @export
#'
#' @examples
get_heroku_pg_url <- function(prop,
                              app,
                              os = "unix") {
  arguments <- create_heroku_pgurl_args(prop = prop,  app = app)

  switch(os,
         "unix"  = run_unix_cmd(arguments = arguments),
         "linux" = run_linux_cmd(arguments = arguments))
}


#' Get con object to Heroku postgres db from url
#'
#' @param db_url URL of Postgres DB
#'
#' @return
#' @export
#'
#' @examples
get_con_from_url <- function(db_url) {
  pg <- httr::parse_url(db_url)
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = trimws(pg$path),
    host = pg$hostname,
    port = pg$port,
    user = pg$username,
    password = pg$password,
    sslmode = "require"
  )
}


#' Get con object to Heroku postgres db from heroku parameters
#'
#' @param prop Property to look for
#' @param app Target Heroku app
#' @param os Target Operating System
#'
#' @return
#' @export
#'
#' @examples
get_con <- function(prop,
                    app,
                    os = "unix") {
  db_url <- get_heroku_pg_url(prop, app, os = "unix")
  get_con_from_url(db_url)
}