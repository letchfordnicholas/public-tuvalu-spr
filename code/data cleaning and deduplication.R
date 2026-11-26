# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# clean & deduplicate: births / deaths / arrivals / departures
# ============================================================

# -----------------------------
# remove all existing data
rm(list = ls())

# -----------------------------
# 0) libraries (minimal set)
# -----------------------------
library(dplyr)      # data manipulation (mutate, filter, select, joins)
library(tidyr)      # data reshaping (unite, pivot, etc.)
library(stringr)    # string helpers (pattern matching, case, trimming)
library(readxl)     # read Excel workbooks
library(janitor)    # clean_names and simple data cleaning utilities
library(haven)      # read Stata/SPSS/SAS files if needed
library(rlang)      # tidy eval helpers (symbols/quoting), used in utilities
library(lubridate)  # dates and times, used by DOB parsing functions
library(here)       # setting file paths for reproducibility

# -----------------------------
# 1) setup paths + helpers
# -----------------------------

# folder names
folder_vital  <- "births and deaths"
folder_immig  <- "immigration"

# file names
birth_file     <- "births_2022+.xlsx"
death_file     <- "deaths_2022+.xlsx"
arrival_file   <- "arrivals_2021+.xlsx"
departure_file <- "departures_2021+.xlsx"

# input files
birth_file_path     <- here("data","raw data",folder_vital,birth_file)
death_file_path     <- here("data","raw data",folder_vital,death_file)
arrival_file_path   <- here("data","raw data",folder_immig,arrival_file)
departure_file_path <- here("data","raw data",folder_immig,departure_file)

# output directories
output_vital_dir <- file.path(here("data","cleaned data",folder_vital))
output_immig_dir <- file.path(here("data","cleaned data",folder_immig))

# create output directories if they do not exist
if (!dir.exists(output_vital_dir)) dir.create(output_vital_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_immig_dir)) dir.create(output_immig_dir, recursive = TRUE, showWarnings = FALSE)

# helper functions
# - data_clean_fn (name/DOB cleaning + fastLink dedup)
# - print_dedup_summary, parse_dob_columns, etc.
source(here("code","code cleaning functions.r"))

# ensure required input files exist
stopifnot(
  file.exists(birth_file_path),
  file.exists(death_file_path),
  file.exists(arrival_file_path),
  file.exists(departure_file_path)
)

# ---------------------------------------------------------------------------
# 2) births: clean + dedup + summary
# ---------------------------------------------------------------------------

birth_data <- data_clean_fn(
  filepath          = birth_file_path,
  combine_name_vars = "Name",
  dob_var           = "DOB",
  dob_format        = "yyyy-mm-dd",
  sex_var           = "Gender",
  address_field     = "Home Island...16",
  linking_var       = "home_island_16",
  dedup_var         = "home_island_16",
  output_file_path  = output_vital_dir,
  output_file_name  = "birth data.csv",
  group_id_var      = "birth_id"
)

birth_data_clean <- birth_data$cleaned_all
print_dedup_summary(birth_data_clean)

# ---------------------------------------------------------------------------
# 3) deaths: clean + dedup + summary
# ---------------------------------------------------------------------------

death_data <- data_clean_fn(
  filepath          = death_file_path,
  dob_var           = "Date of Birth",
  dob_format        = "numeric",
  combine_name_vars = c("Name of Deceased","Surname"),
  sex_var           = "sex",
  linking_var       = c("home_island","date_of_death"),
  dedup_var         = "Home Island",
  output_file_path  = output_vital_dir,
  output_file_name  = "death data.csv",
  group_id_var      = "death_id"
)

death_data_clean <- death_data$cleaned_all
print_dedup_summary(death_data_clean)

# ---------------------------------------------------------------------------
# 4) arrivals: clean + dedup + summary
# ---------------------------------------------------------------------------

arrival_data <- data_clean_fn(
  filepath          = arrival_file_path,
  combine_name_vars = c("FIRST NAME","SURNAME"),
  filter_conditions = list(resident_status = "1"),
  sex_var           = "SEX",
  dob_var           = "DATE OF BIRTH",
  dob_format        = "yyyy-mm-dd",
  linking_var       = "date_of_arrival",
  output_file_path  = output_immig_dir,
  output_file_name  = "arrival data.csv",
  group_id_var      = "arrival_id"
)

arrival_data_clean <- arrival_data$cleaned_all
print_dedup_summary(arrival_data_clean)

# ---------------------------------------------------------------------------
# 5) departures: clean + dedup + summary
# ---------------------------------------------------------------------------

departure_data <- data_clean_fn(
  filepath          = departure_file_path,
  combine_name_vars = c("FIRST NAME","SURNAME"),
  filter_conditions = list(resid_ence = "1"),
  sex_var           = "SEX",
  dob_var           = "DATE OF BIRTH",
  dob_format        = "yyyy-mm-dd",
  linking_var       = "date_of_departure",
  output_file_path  = output_immig_dir,
  output_file_name  = "departure data.csv",
  group_id_var      = "departure_id"
)

departure_data_clean <- departure_data$cleaned_all
print_dedup_summary(departure_data_clean)

