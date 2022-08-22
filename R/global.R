#' @import dplyr
readRenviron(".Renviron")

db_creds <- httr::parse_url(Sys.getenv('DATABASE_URL'))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = db_creds$path,
  host = db_creds$hostname,
  port = db_creds$port,
  user = db_creds$username,
  password = db_creds$password,
  sslmode = "require",
  options = "-c search_path=grain,public,heroku_ext"
)

#library(dplyr)

variety_list <- readr::read_csv("data/variety_coefs.csv")

variety_list <- variety_list %>% 
	filter(is.na(name)) %>% 
	dplyr::arrange(label) 

api_key <- Sys.getenv("MAPS_API_KEY")
max_forecast_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality = 'forecast' ORDER BY date DESC LIMIT 1;")

if (length(max_forecast_date$date) == 0){
	max_forecast_date <- data.frame(date = as.Date("2009-01-01"))
}

max_prism_date <- DBI::dbGetQuery(con, "SELECT DISTINCT(date) FROM grain.prism WHERE quality != 'forecast' ORDER BY date DESC LIMIT 1;")

# switch off s2 geometries 
sf::sf_use_s2(FALSE)