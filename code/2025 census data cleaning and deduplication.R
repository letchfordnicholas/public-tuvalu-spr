# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# 2025 census: load → clean → split names/dob → deduplicate
# ============================================================

# -----------------------------
# remove all existing data
rm(list = ls())

# -----------------------------
# 0) libraries (minimal set)
# -----------------------------
suppressPackageStartupMessages({
  # dplyr/tidyr: core data manipulation and reshaping
  library(dplyr)
  library(tidyr)
  # readxl/haven: import Excel and Stata files
  library(readxl)
  library(haven)
  # janitor: clean column names and simple data cleaning helpers
  library(janitor)
  # stringr: consistent string handling
  library(stringr)
  library(here)
})

# -----------------------------
# 1) setup paths + helpers
# -----------------------------

folder_name <- "2025 census"

file_dwelling  <- "dwelling.dta"
file_household <- "household.dta"
file_person    <- "person.dta"

# input files
dwelling_dta  <- here("data", "raw data", folder_name, file_dwelling)
household_dta <- here("data", "raw data", folder_name, file_household)
person_dta    <- here("data", "raw data", folder_name, file_person)

# outputs
output_dir <- file.path(here("data","cleaned data",folder_name))

# Create output directory if it does not exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# helper functions
# - name parsing/splitting
# - DOB parsing
# - fastLink wrappers and clerical review helpers
source(here("code","code cleaning functions.r"))

# -----------------------------
# 2) load raw census tables
# -----------------------------

# Ensure required files exist
stopifnot(
  file.exists(dwelling_dta),
  file.exists(household_dta),
  file.exists(person_dta)
)

# Dwelling table
# - Extracts GPS coordinates per interview__key
# - Multiple rows per key are reduced to a single record
dwelling_data <- haven::read_dta(dwelling_dta) |>
  dplyr::select(interview__key, dwelling_gps__Latitude, dwelling_gps__Longitude) |>
  dplyr::group_by(interview__key) |>
  dplyr::mutate(row_n = dplyr::row_number()) |>
  dplyr::filter(row_n == 1) |>
  dplyr::ungroup() |>
  dplyr::rename(
    latitude  = dwelling_gps__Latitude,
    longitude = dwelling_gps__Longitude
  )

# Household table
# - Minimal slice to get island code per interview__key
household_data <- haven::read_dta(household_dta) |>
  dplyr::select(interview__key, island_code)

# Person table
# - Main unit records (one row per person)
person_data <- haven::read_dta(person_dta)

# -----------------------------
# 3) harmonise, label, and build `name`
# -----------------------------
# These are the name-related columns that will be concatenated into a single
# `name` field (first and last names, original variants, and other spellings).
cols_to_combine <- c(
  "in_list",
  "p_fnames_o_0","p_fnames_o_1","p_fnames_o_2","p_fnames_o_3","p_fnames_o_4",
  "p_lname",
  "p_lnames_o_0","p_lnames_o_1","p_lnames_o_2","p_lnames_o_3","p_lnames_o_4"
)

person_data_clean <- person_data |>
  # Attach household- and dwelling-level context (island + GPS) to each person
  dplyr::left_join(household_data, by = "interview__key") |>
  dplyr::left_join(dwelling_data,  by = "interview__key") |>
  # Standardise column names to snake_case for easier downstream handling
  janitor::clean_names() |>
  # Convert literal "##n/a##" codes to proper NA for character variables
  dplyr::mutate(across(where(is.character), ~ dplyr::na_if(., "##n/a##"))) |>
  # Convert key categorical variables from labelled (Stata-style) to factor
  # so that we work with human-readable labels rather than numeric codes.
  dplyr::mutate(
    residence_status  = haven::as_factor(residence_status, levels = "label"),
    current_live      = haven::as_factor(current_live,     levels = "label"),
    usual_residence   = haven::as_factor(usual_residence,  levels = "label"),
    resid_2022        = haven::as_factor(resid_2022,       levels = "label"),
    enumeration_area  = haven::as_factor(enum_area,        levels = "label"),
    island            = haven::as_factor(island_code,      levels = "label"),
    village           = haven::as_factor(village_code,     levels = "label"),
    resident          = haven::as_factor(resident,         levels = "label"),
    birthplace        = haven::as_factor(birthplace,       levels = "label"),
    birth_village     = haven::as_factor(birth_village,    levels = "label"),
    relationship_hh   = haven::as_factor(p_relate,         levels = "label"),
    # Collapse multiple "biological child" variants into a single category
    relationship_hh   = dplyr::case_when(
      relationship_hh %in% c(
        "Biological son/daughter of HEAD and SPOUSE",
        "Biological son/daughter of HEAD ONLY",
        "Biological son/daughter of  SPOUSE ONLY"
      ) ~ "Son/Daughter (Tama tagata/Fafine)",
      TRUE ~ relationship_hh
    ),
    # Clean up marital_status labels:
    # - treat "Other" as missing
    # - split "Divorced / Separated" to a cleaner "Divorced" label
    marital_status    = haven::as_factor(marital_status, levels = "label") |> as.character(),
    marital_status    = dplyr::case_when(
      marital_status == "Other"                ~ NA_character_,
      marital_status == "Divorced / Separated" ~ "Divorced",
      TRUE                                     ~ marital_status
    ) |> factor()
  ) |>
  # Normalise all columns:
  # - convert "##N/A##" to NA
  # - let type.convert infer sensible types (numeric, integer, etc.)
  mutate(across(everything(), ~{
    x <- as.character(.x)
    x[x == "##N/A##"] <- NA
    type.convert(x, as.is = TRUE)
  })) |>
  # Build a single `name` field by concatenating first/last name components,
  # dropping NAs and squashing multiple spaces.
  tidyr::unite("name", dplyr::all_of(cols_to_combine), sep = " ", na.rm = TRUE) |>
  dplyr::mutate(name = stringr::str_squish(name)) |>
  # Keep only variables needed for linkage and analysis; drop noisy raw fields
  dplyr::select(
    interview_key, name, p_sex, dob, enumeration_area, village, island,
    birthplace, birth_village, relationship_hh, marital_status,
    residence_status, current_live, usual_residence, resid_2022, resident,
    latitude, longitude
  ) |>
  # Standardise sex variable name; keep original values for recoding later
  dplyr::rename(sex = p_sex) |>
  # Filter out records with missing key attributes for linkage:
  # - sex and usual_residence are required here
  dplyr::filter(!is.na(sex), !is.na(usual_residence)) |>
  # Harmonise birthplace information using village when possible:
  # - if birthplace is "Same village", replace with current village
  # - if "Different village in Tuvalu", use recorded birth_village
  # - if "Overseas", keep as explicit "Overseas" marker
  dplyr::mutate(
    village      = as.character(village),
    birthplace   = as.character(birthplace),
    birth_village= as.character(birth_village),
    birthplace   = dplyr::case_when(
      birthplace == "Different village in Tuvalu" ~ birth_village,
      birthplace == "Same village"                ~ village,
      birthplace == "Overseas"                    ~ "Overseas",
      TRUE                                        ~ birthplace
    ),
    birthplace   = factor(birthplace),
    village      = factor(village)
  ) |>
  # We no longer need birth_village once birthplace has been resolved
  dplyr::select(-birth_village)

# Build a simple crosswalk between village and island using observed
# combinations in the cleaned data. This allows us to infer an island-of-birth
# for cases where only village is known.
village_island_key <- person_data_clean |>
  dplyr::distinct(village, island) |>
  dplyr::rename(
    birth_island = island,
    birthplace   = village
  )

# Attach inferred birth island to person records:
# - join using derived birthplace (village)
# - any unmatched cases are treated as "Outside Tuvalu"
person_data_clean <- person_data_clean |>
  dplyr::left_join(village_island_key, by = "birthplace") |>
  dplyr::mutate(
    birth_island = as.character(birth_island),
    birth_island = dplyr::if_else(is.na(birth_island), "Outside Tuvalu", birth_island),
    birth_island = factor(birth_island)
  ) |>
  dplyr::select(-birthplace) |>
  dplyr::rename(birthplace = birth_island)

# Lowercase selected categorical/string fields to improve stability of
# matching and reduce spurious differences due to case only.
person_data_clean <- lowercase_string_vars(
  person_data_clean,
  c("island","village","relationship_hh","marital_status",
    "residence_status","current_live","usual_residence",
    "resid_2022","resident","birthplace")
)

# -----------------------------
# 4) split names/dob + self-deduplicate
# -----------------------------
# Linking variables:
# - These columns are carried into the fastLink input and the exported
#   clerical review files. They include:
#   * identifiers (interview_key)
#   * cleaned/split names
#   * sex and detailed DOB components
#   * geography (EA, village, island, birthplace)
#   * relationship and residence indicators
#   * GPS coordinates
linking_var <- c(
  "interview_key",
  "name_clean", paste0("name_", 1:8),
  "sex", "dob", "dob_day", "dob_month", "dob_year",
  "enumeration_area","village","island","birthplace",
  "relationship_hh","marital_status",
  "residence_status","current_live","usual_residence","resid_2022","resident",
  "latitude","longitude"
)

# Deduplication variables:
# - Here we are treating `village` and `island` as context for within-area
#   deduplication (alongside standard name/DOB/sex fields managed in data_clean_fn).
dedup_var <- c("village", "island")

# Main cleaning + self-deduplication:
# - Clean name into components
# - Parse DOB into day/month/year
# - Standardise sex values
# - Build fastLink inputs using `linking_var` and `dedup_var`
# - Run self-linking to identify duplicate person records within the 2025 census
# - Export:
#   * full cleaned dataset
#   * deduplicated dataset (one row per group)
#   * duplicate groups for clerical review
census_2025 <- data_clean_fn(
  df               = person_data_clean,
  name_length      = 8,
  name_var         = "name",
  dob_var          = "dob",
  dob_format       = "dd/mm/yyyy",
  sex_var          = "sex",
  sex_key          = c("m" = "Male", "f" = "Female", "1" = "Male", "2" = "Female"),
  linking_var      = linking_var,
  address_field    = "village",
  dedup_var        = dedup_var,
  output_file_path = "data/cleaned data/2025 census",
  output_file_name = "2025 census data.csv"
)

# Extract the deduplicated person-level dataset (one row per dedupe group)
census_2025_dedup <- census_2025$deduplicated_data

# Full cleaned dataset (one row per original record, plus dedup info)
census_2025_cleaned <- census_2025$cleaned_all

# -----------------------------
# 5) summary
# -----------------------------
# Print a simple summary of the deduplication outcome:
# - total records
# - number of potential duplicate records
# - number of unique records
# - number of potentially unique IDs (group heads)
print_dedup_summary(census_2025_cleaned)
