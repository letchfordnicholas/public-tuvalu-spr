# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# build spr database: ingest cleaned/linkable files → sql server
# ============================================================

rm(list = ls())

# -----------------------------
# 0) libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(DBI)
  library(odbc)
  library(here)
})

# -----------------------------
# 1) directories + helpers
# -----------------------------

raw_data_dir     <- here("data","raw data")
clean_data_dir   <- here("data","cleaned data")
linked_data_dir  <- here("data","linked data","spr linkage")

source(here("code","code cleaning functions.r"))

# -----------------------------
# 2) inputs
# -----------------------------

spr_path <- here("data","linked data","spr linkage","spr.csv")

spr <- readr::read_csv(spr_path) %>%
  select(person_id,arrival_id,departure_id,birth_id,death_id)

# -----------------------------
# household data
# -----------------------------

census_file <- here("data","cleaned data","2022 Census","cleaned_all 2022 Census data.csv")

census_data <- readr::read_csv(census_file) |>
  rename(person_id = "census_id") %>%
  dplyr::select(
    person_id,
    contains("name"),
    sex,
    dob,
    dob_year,
    dob_month,
    dob_day,
    interview_key,
    island,
    village,
    enumeration_area,
    latitude,
    longitude,
    current_residence,
    usual_residence,
    marital_status,
    relationship_hh)

# -----------------------------
# births
# -----------------------------

birth_file <- here("data","cleaned data","births and deaths","dedup birth data.csv")

birth_data <- readr::read_csv(birth_file) %>%
  select(birth_id,
         home_island_16) %>%
  rename(mother_home_island = home_island_16,)

# -----------------------------
# deaths
# -----------------------------

death_file <- here("data","cleaned data","births and deaths","cleaned_all death data.csv")

death_data <- readr::read_csv(death_file) |>
  select(
    death_id,
    date_of_death,
    place_of_death,
    island_of_occurrence,
    caused_of_death_or_presumed_a,
    caused_of_death_or_presumed_b,
    caused_of_death_or_presumed_c,
    caused_of_death_or_presumed_d,
    other_significant_conditions,
    stillborn
  ) 

# -----------------------------
# arrivals
# -----------------------------

arrival_file <- here("data","cleaned data","immigration","cleaned_all arrival data.csv")

arrival_data <- readr::read_csv(arrival_file) |>
  select(arrival_id, flight_ship_number, date_of_arrival, citizen_country, passport_number) |>
  parse_dob_columns(dob_var = "date_of_arrival", dob_format = "yyyy-mm-dd") |>
  select(-date_of_arrival) |>
  rename(
    date_of_arrival       = dob,
    date_of_arrival_year  = dob_year,
    date_of_arrival_month = dob_month,
    date_of_arrival_day   = dob_day
  )

# -----------------------------
# departures
# -----------------------------

departure_file <- here("data","cleaned data","immigration","cleaned_all departure data.csv")

departure_data <- readr::read_csv(departure_file) |>
  select(departure_id, flight_ship_number, date_of_departure, passport_number, details_of_other_purpose) |>
  parse_dob_columns(dob_var = "date_of_departure", dob_format = "yyyy-mm-dd") |>
  select(-date_of_departure) |>
  rename(
    date_of_departure       = dob,
    date_of_departure_year  = dob_year,
    date_of_departure_month = dob_month,
    date_of_departure_day   = dob_day
  )

# -----------------------------
# 3) sql server write tables
# -----------------------------

server_con <- dbConnect(
  odbc(),
  Driver             = "ODBC Driver 17 for SQL Server",
  Server             = "localhost\\sqlexpress",
  Trusted_Connection = "Yes",
  Encrypt            = "no"
)

db_name <- "spr"

exists_q <- sprintf(
  "select 1 from sys.databases where name = %s",
  dbQuoteString(server_con, db_name)
)

exists <- dbGetQuery(server_con, exists_q)

if (nrow(exists) == 0) {
  dbExecute(server_con,
            paste0("create database ", dbQuoteIdentifier(server_con, db_name)))
}

con <- dbConnect(
  odbc(),
  Driver             = "ODBC Driver 17 for SQL Server",
  Server             = "localhost\\sqlexpress",
  Database           = db_name,
  Trusted_Connection = "Yes",
  Encrypt            = "no"
)

on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
on.exit(try(dbDisconnect(server_con), silent = TRUE), add = TRUE)

dbWriteTable(con, "spr_tbl", spr, overwrite = TRUE)
dbWriteTable(con, "birth_tbl", birth_data, overwrite = TRUE)
dbWriteTable(con, "death_tbl", death_data_, overwrite = TRUE)
dbWriteTable(con, "census_tbl", census_data, overwrite = TRUE)
dbWriteTable(con, "arrival_tbl", arrival_data, overwrite = TRUE)
dbWriteTable(con, "departure_tbl", departure_data, overwrite = TRUE)

 