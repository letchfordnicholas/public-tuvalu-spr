# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# 2022 ↔ 2025 census linkage in iterative batches
# ============================================================

rm(list = ls())

# -----------------------------
# 0) libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(readr)
  library(ggplot2)
  library(tibble)
  library(here)
})

# -----------------------------
# 1) helpers + directories
# -----------------------------

clean_data_dir  <- here("data","cleaned data")
linked_data_dir <- here("data","linked data","census data linkage")

clean_data_dir_a <- "2022 Census"
clean_data_dir_b <- "2025 Census"

clean_data_a_name <- "2022 Census data"
clean_data_b_name <- "2025 Census data"

df_a_groupid_name <- "census_2022_id"
df_b_groupid_name <- "census_2025_id"

if (!dir.exists(linked_data_dir))
  dir.create(linked_data_dir, recursive = TRUE, showWarnings = FALSE)

source(here("code","code cleaning functions.r"))

# -----------------------------
# 2) data prep
# -----------------------------

census_2022 <- load_and_process_data(
  file.path(clean_data_dir,clean_data_dir_a),
  clean_data_a_name
) |>
  select(-usual_residence) |>
  rename(!!df_a_groupid_name := census_id)

census_2025 <- load_and_process_data(
  file.path(clean_data_dir,clean_data_dir_b),
  clean_data_b_name
) |>
  select(-resid_2022,-resident,-residence_status) |>
  rename(!!df_b_groupid_name := group_id) |>
  filter(dob_year <= 2022)

# ============================================================
# 3) BATCH 1 — richer name keys
# ============================================================

batch1_sample_fraction_review <- 0.1
batch1_file <- paste0("1st batch 2022-2025 census linkage - ",batch1_sample_fraction_review,".xlsx")
batch1_path <- here("data","linked data","census data linkage",batch1_file)

linking_var_1 <- c(
  "name_clean","name_1","name_2","name_3","name_4","name_5",
  "dob_day","dob_month","dob_year","sex","village","island"
)

string_linking_vars =c(
  "name_clean","name_1","name_2","name_3",
  "name_4","name_5","village","island")

batch1_prob_thres <- 0.9

generate_cr_file <- FALSE

nls_1 <- run_fastlink(
  linking_vars = linking_var_1,
  df_a = census_2022,
  df_b = census_2025,
  stringdist_vars = string_linking_vars,
  rowid_a = df_a_groupid_name,
  rowid_b = df_b_groupid_name
)

res_1 <- link_join_and_reshape(
  nls = nls_1,
  dfA = census_2022,
  dfB = census_2025,
  linking_var = linking_var_1,
  sample_fraction = batch1_sample_fraction_review,
  generate_xlsx = generate_cr_file,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  output_path = batch1_path,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

cr_1 <- read_xlsx(batch1_path)

summarise_cr_data(
  cr_data = cr_1,
  nls = nls_1,
  dfA = census_2022,
  dfB = census_2025,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

lu_1 <- build_linked_unlinked(
  cr_data = cr_1,
  nls = nls_1,
  dfA = census_2022,
  dfB = census_2025,
  post_threshold = batch1_prob_thres,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

batch_1_linked <- lu_1$linked_data
census_2022_unlinked <- lu_1$dfA_unlinked_data
census_2025_unlinked <- lu_1$dfB_unlinked_data

analyse_missing_combinations(census_2022_unlinked,linking_var_1)
analyse_missing_combinations(census_2025_unlinked,linking_var_1)

# ============================================================
# 4) BATCH 2 — shorter name keys
# ============================================================

batch2_sample_fraction_review <- 0.3
batch2_file <- paste0("2nd batch 2022-2025 census linkage.xlsx")
batch2_path <- here("data","linked data","census data linkage",batch2_file)

linking_var_2 <- c(
  "name_1","name_2","name_3",
  "dob_day","dob_month","dob_year",
  "sex","village","island"
)

string_linking_vars <- c("name_1","name_2","name_3","village","island")

batch2_prob_thres <- 0.9

nls_2 <- run_fastlink(
  linking_vars = linking_var_2,
  df_a = census_2022_unlinked,
  df_b = census_2025_unlinked,
  stringdist_vars = string_linking_vars,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

res_2 <- link_join_and_reshape(
  nls = nls_2,
  dfA = census_2022_unlinked,
  dfB = census_2025_unlinked,
  linking_var = linking_var_2,
  sample_fraction = batch2_sample_fraction_review,
  generate_xlsx = generate_cr_file,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  output_path = batch2_path,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

cr_2 <- read_xlsx(batch2_path)

summarise_cr_data(
  cr_data = cr_2,
  nls = nls_2,
  dfA = census_2022_unlinked,
  dfB = census_2025_unlinked,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

lu_2 <- build_linked_unlinked(
  cr_data = cr_2,
  nls = nls_2,
  dfA = census_2022_unlinked,
  dfB = census_2025_unlinked,
  post_threshold = 0.9,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

batch_2_linked <- lu_2$linked_data
census_2022_2_unlinked <- lu_2$dfA_unlinked_data
census_2025_2_unlinked <- lu_2$dfB_unlinked_data

# ============================================================
# 5) BATCH 3 — remove jr/jnr tokens
# ============================================================

batch3_sample_fraction_review <- 0.5

linking_var_2 <- c(
  "name_1","name_2","name_3",
  "dob_day","dob_month","dob_year",
  "sex","village","island"
)

string_linking_vars <- c("name_1","name_2","name_3","village","island")

batch3_prob_thres <- 0.9

batch3_file <- "2022-2025 census linkage - remove jnr from names.xlsx"
batch3_path <- here("data","linked data","census data linkage",batch3_file)

census_2022_2_unlinked_jr <- shift_out_junior(census_2022_2_unlinked)
census_2025_2_unlinked_jr <- shift_out_junior(census_2025_2_unlinked)

nls_3 <- run_fastlink(
  linking_vars = linking_var_2,
  df_a = census_2022_2_unlinked_jr,
  df_b = census_2025_2_unlinked_jr,
  stringdist_vars = string_linking_vars,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

res_3 <- link_join_and_reshape(
  nls = nls_3,
  dfA = census_2022_2_unlinked_jr,
  dfB = census_2025_2_unlinked_jr,
  linking_var = linking_var_2,
  sample_fraction = batch3_sample_fraction_review,
  generate_xlsx = generate_cr_file,
  output_path = batch3_path,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

cr_3 <- read_xlsx(batch3_path)

summarise_cr_data(
  cr_data = cr_3,
  nls = nls_3,
  dfA = census_2022_2_unlinked_jr,
  dfB = census_2025_2_unlinked_jr,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

lu_3 <- build_linked_unlinked(
  cr_data = cr_3,
  nls = nls_3,
  dfA = census_2022_2_unlinked_jr,
  dfB = census_2025_2_unlinked_jr,
  post_threshold = batch3_prob_thres,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

batch_3_linked <- lu_3$linked_data
census_2022_3_unlinked <- lu_3$dfA_unlinked_data
census_2025_3_unlinked <- lu_3$dfB_unlinked_data


# ============================================================
# 6) BATCH 4 — GPS linkage
# ============================================================

batch4_sample_fraction_review <- 0.5
batch4_file <- "4th batch 2022-2025 census linkage.xlsx"
batch4_path <- here("data","linked data","census data linkage",batch4_file)

linking_var_4 <- c(
  "name_1","name_2","dob_day","dob_month","dob_year",
  "sex","relationship_hh","birthplace","longitude","latitude"
)

string_linking_vars <- c("name_1","name_2","relationship_hh","birthplace")

batch4_prob_thres <- 0.99

census_2022_3_unlinked_jr <- shift_out_junior(census_2022_3_unlinked)
census_2025_3_unlinked_jr <- shift_out_junior(census_2025_3_unlinked)

nls_4 <- run_fastlink(
  linking_vars = linking_var_4,
  df_a = census_2022_3_unlinked_jr,
  df_b = census_2025_3_unlinked_jr,
  stringdist_vars = string_linking_vars,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

res_4 <- link_join_and_reshape(
  nls = nls_4,
  dfA = census_2022_3_unlinked_jr,
  dfB = census_2025_3_unlinked_jr,
  linking_var = linking_var_4,
  sample_fraction = batch4_sample_fraction_review,
  generate_xlsx = generate_cr_file,
  output_path = batch4_path,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

cr_4 <- read_xlsx(batch4_path)

summarise_cr_data(
  cr_data = cr_4,
  nls = nls_4,
  dfA = census_2022_3_unlinked_jr,
  dfB = census_2025_3_unlinked_jr,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

lu_4 <- build_linked_unlinked(
  cr_data = cr_4,
  nls = nls_4,
  dfA = census_2022_3_unlinked_jr,
  dfB = census_2025_3_unlinked_jr,
  post_threshold = batch4_prob_thres,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name,
  thresholds = seq(0.9, 0.99, 0.01)
)

batch_4_linked <- lu_4$linked_data
census_2022_4_unlinked <- lu_4$dfA_unlinked_data
census_2025_4_unlinked <- lu_4$dfB_unlinked_data

# ============================================================
# 7) BATCH 5 — enumeration area linkage
# ============================================================

batch5_sample_fraction_review <- 1
batch5_file <- "5th batch 2022-2025 census linkage.xlsx"
batch5_path <- here("data","linked data","census data linkage",batch5_file)

linking_var_5 <- c(
  "name_1","dob_day","dob_month","dob_year",
  "sex","birthplace","relationship_hh","enumeration_area"
)

string_linking_vars <- c("name_1","sex","birthplace","relationship_hh")
 
batch5_prob_thres <- 1

census_2022_4_unlinked_jr <- shift_out_junior(census_2022_4_unlinked) |>
  mutate(enumeration_area = suppressWarnings(as.numeric(enumeration_area)))
census_2025_4_unlinked_jr <- shift_out_junior(census_2025_4_unlinked) |>
  mutate(enumeration_area = suppressWarnings(as.numeric(enumeration_area)))

nls_5 <- run_fastlink(
  linking_vars = linking_var_5,
  df_a = census_2022_4_unlinked_jr,
  df_b = census_2025_4_unlinked_jr,
  stringdist_vars = string_linking_vars,
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

res_5 <- link_join_and_reshape(
  nls = nls_5,
  dfA = census_2022_4_unlinked_jr,
  dfB = census_2025_4_unlinked_jr,
  linking_var = linking_var_5,
  sample_fraction = batch5_sample_fraction_review,
  generate_xlsx = generate_cr_file,
  output_path = batch5_path,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

cr_5 <- read_xlsx(batch5_path)

summarise_cr_data(
  cr_data = cr_5,
  nls = nls_5,
  dfA = census_2022_4_unlinked_jr,
  dfB = census_2025_4_unlinked_jr,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name,
  thresholds = seq(0.9, 0.99, 0.01)
)

lu_5 <- build_linked_unlinked(
  cr_data = cr_5,
  nls = nls_5,
  dfA = census_2022_4_unlinked_jr,
  dfB = census_2025_4_unlinked_jr,
  post_threshold = batch5_prob_thres,
  source_label = c(data_source_a=clean_data_dir_a,data_source_b=clean_data_dir_b),
  rowid_a=df_a_groupid_name,
  rowid_b=df_b_groupid_name
)

batch_5_linked <- lu_5$linked_data
census_2022_5_unlinked <- lu_5$dfA_unlinked_data
census_2025_5_unlinked <- lu_5$dfB_unlinked_data

# linkaged results are getting too poor quality to link further records with sufficient accuracy
# decided to stop linkage analysis

# ============================================================
# 8) diagnostics for unlinked sets
# ============================================================

unlinked_dir <- here("data","linked data","census data linkage","unlinked data")

if (!dir.exists(unlinked_dir))
  dir.create(unlinked_dir, recursive = TRUE)

write_csv(
  census_2022_5_unlinked,
  file.path(unlinked_dir,"2022 census unlinked data.csv")
)

write_csv(
  census_2025_5_unlinked,
  file.path(unlinked_dir,"2025 census unlinked data.csv")
)

# ============================================================
# 9) combine all linked + coverage
# ============================================================

combined_linked_data <- bind_rows(
  batch_1_linked,
  batch_2_linked,
  batch_3_linked,
  batch_4_linked,
  batch_5_linked
)

overall_linkage_rate <-
  nrow(combined_linked_data) /
  min(nrow(census_2025),nrow(census_2022)) * 100

overall_linkage_rate

