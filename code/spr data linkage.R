# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# spr build: link census ↔ arrivals ↔ departures ↔ deaths
# ============================================================

rm(list = ls())

# -----------------------------
# 0) libraries
# -----------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(cowplot)
  library(readxl)
  library(here)
})

# -----------------------------
# 1) directories + helpers
# -----------------------------

clean_data_dir  <- here("data","cleaned data")
linked_data_dir <- here("data","linked data","spr linkage")
fig_dir         <- here("figures")

if (!dir.exists(linked_data_dir))
  dir.create(linked_data_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(fig_dir))
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

source(here("code","code cleaning functions.r"))

# -----------------------------
# 2) load cleaned datasets
# -----------------------------

census_folder_path <- "2022 Census"
census_file_path <- "2022 Census data"
census_id_code <- "census_id"

census_2022 <- load_and_process_data(
  file.path(clean_data_dir,census_folder_path),
  census_file_path
) |>
  select(-usual_residence)

arrival_file_path <- "arrival data"
immigration_folder_path <- "immigration"
arrival_id_code <- "arrival_id"

arrival_data <- load_and_process_data(
  file.path(clean_data_dir,immigration_folder_path),
  arrival_file_path
)

departure_file_path <- "departure data"
departure_id_code <- "departure_id"

departure_data <- load_and_process_data(
  file.path(clean_data_dir,immigration_folder_path),
  departure_file_path
)

birth_death_folder_path <- "births and deaths"
birth_file_path <- "birth data"
birth_id_code <- "birth_id"

birth_data <- load_and_process_data(
  file.path(clean_data_dir,birth_death_folder_path),
  birth_file_path
) |>
  mutate(
    birthplace    = home_island_16,
    village       = NA,
    island        = NA,
    census_id     = NA,
    arrival_id    = NA,
    interview_key = NA
  ) |>
  select(-home_island_16) |>
  filter(!is.na(name_clean))

death_file_path <- "death data"
death_id_code <- "death_id"

death_data <- load_and_process_data(
  file.path(clean_data_dir,birth_death_folder_path),
  death_file_path
) |>
  rename(island = home_island) |>
  filter(!is.na(name_clean))

# ============================================================
# 3) link arrivals to census
# ============================================================

generate_cr_file <- FALSE

arr_link_file <- "2022 census - arrival data linkage - 0.1.xlsx"
arr_link_path <- here("data","linked data","spr linkage",arr_link_file)

linking_vars <- c("name_clean","name_1","name_2","name_3","name_4","name_5",
  "dob_day","dob_month","dob_year","sex")

string_linking_vars <- c("name_clean","name_1","name_2","name_3","name_4","name_5")

nls_arr <- run_fastlink(
  linking_vars = linking_vars,
  df_a = census_2022,
  df_b = arrival_data,
  stringdist_vars = string_linking_vars,
  rowid_a = census_id_code,
  rowid_b = arrival_id_code
)

res_arr <- link_join_and_reshape(
  nls = nls_arr,
  dfA = census_2022,
  dfB = arrival_data,
  linking_var = linking_vars,
  sample_fraction = 0.1,
  generate_xlsx = generate_cr_file,
  source_label = c(data_source_a=census_folder_path,data_source_b=arrival_file_path),
  output_path = arr_link_path,
  rowid_a=census_id_code,
  rowid_b=arrival_id_code
)

cr_arr <- read_xlsx(arr_link_path)

summarise_cr_data(
  cr_data = cr_arr,
  nls = nls_arr,
  dfA = census_2022,
  dfB = arrival_data,
  source_label = c(data_source_a=census_folder_path,data_source_b=arrival_file_path),
  rowid_a=census_id_code,
  rowid_b=arrival_id_code
)

lu_arr <- build_linked_unlinked(
  cr_data = cr_arr,
  nls = nls_arr,
  dfA = census_2022,
  dfB = arrival_data,
  post_threshold = 0,
  source_label = c(data_source_a=census_folder_path,data_source_b=arrival_file_path),
  rowid_a=census_id_code,
  rowid_b=arrival_id_code
)

census_arrival_linked <- lu_arr$linked_data |>
  left_join(census_2022, by=census_id_code) |>
  select(-enumeration_area,-current_residence,-marital_status,
         -relationship_hh,-longitude,-latitude)

census_unlinked <- lu_arr$dfA_unlinked_data |>
  mutate(arrival_id = NA, birth_id = NA) |>
  select(-enumeration_area,-current_residence,-marital_status,
         -relationship_hh,-longitude,-latitude)

arrival_unlinked <- lu_arr$dfB_unlinked_data |>
  mutate(
    village       = NA,
    island        = NA,
    birthplace    = NA,
    birth_id      = NA,
    census_id     = NA,
    interview_key = NA
  ) |>
  select(-date_of_arrival)

census_arrival_combined <- census_arrival_linked |>
  bind_rows(census_unlinked) |>
  bind_rows(arrival_unlinked) |>
  bind_rows(birth_data) |>
  select(-rowid) |>
  mutate(
    census_id = case_when(
      is.na(census_id) & !is.na(birth_id)   ~ max(census_id, na.rm=TRUE)+birth_id,
      is.na(census_id) & !is.na(arrival_id) ~ max(census_id, na.rm=TRUE)+arrival_id,
      TRUE ~ census_id
    )
  ) |>
  rowid_to_column("rowid")

# ============================================================
# 4) link departures to SPR
# ============================================================

dep_link_file <- "2022 census - departure data linkage - 0.1.xlsx"
dep_link_path <- here("data","linked data","spr linkage",dep_link_file)

linking_vars <- c("name_clean","name_1","name_2","name_3","name_4","name_5",
                  "dob_day","dob_month","dob_year","sex")

string_linking_vars <- c("name_clean","name_1","name_2","name_3","name_4","name_5")

nls_dep <- run_fastlink(
  linking_vars = linking_vars,
  df_a = census_arrival_combined,
  df_b = departure_data,
  stringdist_vars = string_linking_vars,
  rowid_a="rowid",
  rowid_b=departure_id_code
)

res_dep <- link_join_and_reshape(
  nls = nls_dep,
  dfA = census_arrival_combined,
  dfB = departure_data,
  linking_var = linking_vars,
  sample_fraction = 0.1,
  generate_xlsx = generate_cr_file,
  source_label = c(data_source_a="spr",data_source_b=departure_file_path),
  output_path = dep_link_path,
  rowid_a="rowid",
  rowid_b=departure_id_code
)

cr_dep <- read_xlsx(dep_link_path)

lu_dep <- build_linked_unlinked(
  cr_data = cr_dep,
  nls = nls_dep,
  dfA = census_arrival_combined,
  dfB = departure_data,
  post_threshold = 0.8,
  source_label = c(data_source_a="spr",data_source_b=departure_file_path),
  rowid_a="rowid",
  rowid_b=departure_id_code
)

departure_linked <- lu_dep$linked_data |>
  inner_join(census_arrival_combined, by="rowid") |>
  filter(!is.na(census_id) | !is.na(arrival_id))

# ============================================================
# 5) link deaths to SPR
# ============================================================

death_link_file <- "2022 census - death data linkage.xlsx"
death_link_path <- here("data","linked data","spr linkage",death_link_file)

linking_vars_death <- c("name_clean","name_1","name_2","name_3","name_4","name_5",
  "dob_day","dob_month","dob_year","sex","island")

string_linking_vars <- c("name_clean","name_1","name_2","name_3","name_4","name_5")

nls_die <- run_fastlink(
  linking_vars = linking_vars_death,
  df_a = census_arrival_combined,
  df_b = death_data,
  stringdist_vars = string_linking_vars,
  rowid_a="rowid",
  rowid_b=death_id_code
)

res_die <- link_join_and_reshape(
  nls = nls_die,
  dfA = census_arrival_combined,
  dfB = death_data,
  linking_var = linking_vars_death,
  sample_fraction = 1,
  generate_xlsx = generate_cr_file,
  source_label = c(data_source_a="spr",data_source_b=death_file_path),
  output_path = death_link_path,
  rowid_a="rowid",
  rowid_b=death_id_code
)

cr_die <- read_xlsx(death_link_path)

lu_die <- build_linked_unlinked(
  cr_data = cr_die,
  nls = nls_die,
  dfA = census_arrival_combined,
  dfB = death_data,
  post_threshold = 0,
  source_label = c(data_source_a="spr",data_source_b=death_file_path),
  rowid_a="rowid",
  rowid_b=death_id_code
)

death_linked <- lu_die$linked_data |>
  inner_join(census_arrival_combined, by="rowid") |>
  filter(!is.na(census_id) | !is.na(arrival_id) | !is.na(birth_id))

# ============================================================
# 6) build final SPR
# ============================================================

spr_file <- here("data","linked data","spr linkage","spr.csv")

spr <- census_arrival_combined |>
  select(-village,-island) |>
  left_join(
    census_2022 |> select(census_id,marital_status,current_residence,relationship_hh),
    by=census_id_code
  ) |>
  left_join(
    death_linked |> select(death_id,census_id,arrival_id,birth_id),
    by=c(census_id_code,arrival_id_code,birth_id_code)
  ) |>
  left_join(
    departure_linked |> select(departure_id,census_id,arrival_id,birth_id),
    by=c(census_id_code,arrival_id_code,birth_id_code)
  ) |>
  rename(person_id = census_id)

write_csv(spr, spr_file)

# ============================================================
# 7) figure: monthly contributions + population line
# ============================================================

# try loading 2025 census size
census_2025_n <- tryCatch(
  load_and_process_data(
    file.path(clean_data_dir,"2025 Census"),
    "2025 Census data"
  ) |> nrow(),
  error = function(e) NA_integer_
)

census_date <- as.Date("2025-01-01")

# attach event dates to SPR
pop_timeline <- spr |>
  left_join(arrival_data   |> select(arrival_id, date_of_arrival),   by = arrival_id_code) |>
  left_join(departure_data |> select(departure_id, date_of_departure), by = departure_id_code) |>
  left_join(birth_data     |> select(birth_id, dob), by = birth_id_code) |>
  left_join(death_data     |> select(death_id, date_of_death), by = death_id_code) |>
  mutate(
    dob               = suppressWarnings(as.Date(dob)),
    date_of_arrival   = suppressWarnings(as.Date(date_of_arrival)),
    date_of_departure = suppressWarnings(as.Date(date_of_departure)),
    date_of_death     = suppressWarnings(as.Date(date_of_death))
  )

initial_pop <- pop_timeline |>
  summarise(n = n_distinct(person_id, na.rm = TRUE)) |>
  pull(n)

events <- bind_rows(
  pop_timeline |>
    distinct(arrival_id, date_of_arrival) |>
    filter(!is.na(arrival_id), !is.na(date_of_arrival)) |>
    transmute(date = date_of_arrival, type = "arrival", delta = 1L),
  
  pop_timeline |>
    distinct(departure_id, date_of_departure) |>
    filter(!is.na(departure_id), !is.na(date_of_departure)) |>
    transmute(date = date_of_departure, type = "departure", delta = -1L),
  
  pop_timeline |>
    distinct(birth_id, dob) |>
    filter(!is.na(birth_id), !is.na(dob)) |>
    transmute(date = dob, type = "birth", delta = 1L),
  
  pop_timeline |>
    distinct(death_id, date_of_death) |>
    filter(!is.na(death_id), !is.na(date_of_death)) |>
    transmute(date = date_of_death, type = "death", delta = -1L)
) |>
  filter(!is.na(date)) |>
  mutate(date = as.Date(date))

monthly_contrib <- events |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month, type) |>
  summarise(delta = sum(delta), .groups = "drop")

monthly_total <- monthly_contrib |>
  group_by(month) |>
  summarise(delta = sum(delta), .groups = "drop") |>
  arrange(month)

if (nrow(monthly_total) == 0) {
  warning("no dated events found; skipping plot export.")
} else {
  
  all_months <- tibble(
    month = seq(min(monthly_total$month),
                max(monthly_total$month),
                by = "1 month")
  )
  
  pop_by_month <- all_months |>
    left_join(monthly_total, by = "month") |>
    mutate(
      delta = replace_na(delta, 0L),
      population = initial_pop + cumsum(delta),
      pop_start = lag(population, default = initial_pop)
    )
  
  monthly_contrib_offset <- monthly_contrib |>
    left_join(pop_by_month |> select(month, pop_start), by = "month") |>
    mutate(ymin = pop_start, ymax = pop_start + delta)
  
  monthly_contrib_plot <- monthly_contrib_offset |>
    mutate(type = factor(type,
                         levels = c("birth","arrival","death","departure")))
  
  x_min <- min(monthly_contrib_plot$month, na.rm = TRUE)
  x_max <- max(monthly_contrib_plot$month, na.rm = TRUE)
  
  x_breaks <- seq(as.Date(cut(x_min, "month")),
                  as.Date(cut(x_max, "month")),
                  by = "3 months")
  
  y_range <- range(pop_by_month$population, na.rm = TRUE)
  y_breaks <- pretty(y_range, n = 6)
  
  p_bars <- ggplot(monthly_contrib_plot,
                   aes(x = month, y = delta, fill = type)) +
    geom_col(width = 25) +
    scale_x_date(limits = c(x_min, x_max),
                 breaks = x_breaks,
                 date_labels = "%Y-%b") +
    scale_y_continuous(name = "monthly population\nchange") +
    scale_fill_manual(
      values = c(
        birth = "#0072B2",
        arrival = "#D55E00",
        departure = "#009E73",
        death = "#E82902"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    )
  
  census_pt <- if (!is.na(census_2025_n)) {
    data.frame(month = census_date, population = census_2025_n)
  } else NULL
  
  p_pop <- ggplot(pop_by_month,
                  aes(x = month, y = population)) +
    geom_line(linewidth = 1, colour = "black") +
    geom_point(size = 1.4) +
    {if(!is.null(census_pt))
      geom_point(data = census_pt,
                 aes(x = month, y = population),
                 colour = "red", size = 4)} +
    scale_x_date(limits = c(x_min, x_max),
                 breaks = x_breaks,
                 date_labels = "%Y-%b") +
    scale_y_continuous(name = "cumulative\npopulation",
                       breaks = y_breaks) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined_plot <- cowplot::plot_grid(
    p_bars,
    p_pop,
    ncol = 1,
    align = "v",
    axis = "l"
  )
}

