# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# tuvalu 2022 census: load, clean, split names/dob, deduplicate
# ============================================================

# -----------------------------
# remove all existing data
rm(list = ls())

# -----------------------------
# 0) libraries (quietly load)
# -----------------------------
library(dplyr)      # core data wrangling (mutate, filter, select, joins)
library(tidyr)      # reshaping and column combining (unite, pivot, etc.)
library(stringr)    # string manipulation helpers (str_to_lower, str_squish)
library(readxl)     # read Excel files for housing/household data
library(janitor)    # clean_names and simple cleaning utilities
library(haven)      # read Stata (.dta) files with labels
library(rlang)      # tidy evaluation helpers (symbols, quosures, etc.)
library(lubridate)  # dates (not heavily used here but loaded for consistency)
library(here)       # setting file paths for reproducibility

# ---------------------------------------
# 1) configuration (paths & functions)
# ---------------------------------------
folder_name <- "2022 Census"
file_name_1 <- "TuvaluPHC2022_Housing.xlsx"
file_name_2 <- "TUVLFC2022_person.dta"

# input files
census_hh_xlsx <- here("data", "raw data", folder_name,file_name_1)
census_pers_dta <- here("data", "raw data", folder_name,file_name_2)

# outputs (directory must exist for csvs written by data_clean_fn)
output_dir <- file.path(here("data","cleaned data",folder_name))
# Create output directory for cleaned/deduped CSVs if it does not already exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# helper functions (from your earlier cleaned utilities)
# - data_clean_fn (name/DOB cleaning, fastLink dedup)
# - print_dedup_summary, lowercase_string_vars, etc.
source(here("code","code cleaning functions.r"))

# -----------------------------
# 2) load raw source datasets
# -----------------------------
# Ensure that both input files exist before proceeding
stopifnot(file.exists(census_hh_xlsx), file.exists(census_pers_dta))

# household gps + interview key
# - Read housing file
# - Standardise column names
# - Keep only interview key and GPS coordinates
household_data <- read_excel(census_hh_xlsx) |>
  janitor::clean_names() |>
  dplyr::select(
    interview_key = interview_key,          # ensure consistent name for join key
    latitude      = building_gps_latitude,  # rename to generic latitude
    longitude     = building_gps_longitude  # rename to generic longitude
  )

# person records
# - Read person-level file (Stata)
# - Clean column names for consistency
person_data_2022 <- haven::read_dta(census_pers_dta) |>
  janitor::clean_names()

# -------------------------------------------
# 3) harmonise labels and construct name field
# -------------------------------------------
# add more parts here later if available (e.g., given, middle, family)
# For now, the name field is based on the household member label (hh_member)
cols_to_combine <- c("hh_member")

person_data_clean_2022 <- person_data_2022 |>
  # Attach GPS coordinates from household_data using interview_key
  dplyr::left_join(household_data, by = "interview_key") |>
  # Convert literal "##n/a##" codes in character fields to proper NA values
  dplyr::mutate(across(where(is.character), ~ dplyr::na_if(., "##n/a##"))) |>
  # Convert labelled Stata fields to factors, then to lowercase character where needed
  dplyr::mutate(
    current_residence = haven::as_factor(current_residence, levels = "label") |> stringr::str_to_lower(),
    usual_residence   = haven::as_factor(usual_residence,   levels = "label") |> stringr::str_to_lower(),
    island            = haven::as_factor(island,            levels = "label") |> stringr::str_to_lower(),
    village           = haven::as_factor(village,           levels = "label") |> stringr::str_to_lower(),
    marital_status    = haven::as_factor(mstatus,           levels = "label") |> as.character() |> stringr::str_to_lower(),
    # Clean up marital status categories for more consistent labels
    marital_status    = dplyr::case_when(
      marital_status == "single / never married" ~ "never married",
      marital_status == "divorced / separated"   ~ "divorced",
      marital_status == "common-law / live-in"   ~ "defacto relationship",
      marital_status == "unknown"                ~ NA_character_,
      TRUE                                       ~ marital_status
    ) |> factor(),
    # Standardise relationship-to-head labels to lowercase
    relationship_hh  = haven::as_factor(relat,     levels = "label") |> stringr::str_to_lower(),
    # Standardise birthplace labels and enumeration areas to lowercase strings
    birthplace       = haven::as_factor(birthplace,levels = "label") |> stringr::str_to_lower(),
    enumeration_area = haven::as_factor(ea,        levels = "label") |> stringr::str_to_lower()
  ) |>
  # Build a single 'name' column from hh_member (and later additional parts if available)
  tidyr::unite("name", dplyr::all_of(cols_to_combine), sep = " ", na.rm = TRUE) |>
  # Trim and lowercase the combined name string for consistent matching
  dplyr::mutate(name = stringr::str_squish(name) |> stringr::str_to_lower()) |>
  # Keep only variables needed for linkage and analysis
  dplyr::select(
    interview_key, name, sex, dob,
    enumeration_area, village, island, birthplace,
    current_residence, usual_residence,
    marital_status, relationship_hh,
    longitude, latitude
  )

# ---------------------------------------------------------
# 4) linking variables & deduplication variable sets
# ---------------------------------------------------------
# linking_var:
# - fields to keep through cleaning/dedup and to use for linkage
# - includes:
#    * interview_key (record identifier)
#    * cleaned/split name fields
#    * sex and full DOB breakdown
#    * geographic context (EA, village, island, birthplace)
#    * residence status and marital/relationship fields
#    * GPS coordinates
linking_var <- c(
  "interview_key",
  "name_clean", paste0("name_", 1:5),
  "sex", "dob", "dob_day", "dob_month", "dob_year",
  "enumeration_area","village","island","birthplace",
  "current_residence","usual_residence",
  "marital_status","relationship_hh",
  "longitude","latitude"
)

# dedup_var:
# - variables used by fastLink for self-deduplication
# - here we use village + island as contextual dedupe keys
dedup_var <- c("village", "island")

# ---------------------------------------------
# 5) clean/split names & dob, then self-dedup
# ---------------------------------------------
# data_clean_fn:
# - splits and cleans the 'name' column into name_clean and name_1..name_5
# - parses 'dob' into dob_day/dob_month/dob_year
# - standardises sex codes based on sex_key
# - runs fastLink self-linking using dedup_var and linking_var
# - writes three CSVs:
#     * cleaned_all
#     * dedup {output_file_name}
#     * duplicates {output_file_name}
# - returns a list including cleaned_all and deduplicated_data
census_2022 <- data_clean_fn(
  df               = person_data_clean_2022,      # in-memory cleaned person data
  name_var         = "name",                      # column containing combined name
  name_length      = 5,                           # number of name components to derive
  dob_var          = "dob",                       # raw DOB column
  dob_format       = "dd/mm/yyyy",                # expected DOB format (day-first)
  sex_var          = "sex",                       # column containing sex codes
  sex_key          = c("m" = "Male", "f" = "Female", "1" = "Male", "2" = "Female"),
  linking_var      = linking_var,                 # extra variables to carry into outputs
  dedup_var        = dedup_var,                   # variables to help fastLink dedup
  output_file_path = output_dir,                  # target folder for CSV outputs
  output_file_name = "2022 census data.csv",      # base filename for exported CSVs
  group_id_var     = "census_id"                  # name of dedupe group id field
)

# Full cleaned dataset (one row per original record, plus dedup info)
census_2022_cleaned <- census_2022$cleaned_all
# Deduplicated dataset (one representative row per dedupe group)
census_2022_dedup   <- census_2022$deduplicated_data

# -------------------------------
# 6) quick deduplication summary
# -------------------------------
# Print high-level counts:
# - total records
# - number of potential duplicate records
# - number of unique records
# - number of potentially unique IDs
print_dedup_summary(census_2022_cleaned)
