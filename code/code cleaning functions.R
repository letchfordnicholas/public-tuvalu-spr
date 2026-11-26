# ============================================================
# author: Nicholas Letchford
# date: 27/9/2025
# project: Data Integration for Tuvalu Statistical Population Register
# FJ-SPC-469891-CS-INDV – REOI25-7673
# ============================================================
# Utilities for name cleaning, DOB parsing, deduping (fastLink),
# clerical-review helpers, SQL upserts, and plotting helpers
# ============================================================

# ---- Packages (explicit namespaces used below; keep as reference) ----
# library(dplyr); library(stringr); library(tidyr); library(purrr)
# library(lubridate); library(janitor); library(rlang); library(tibble)
# library(fastLink); library(DBI); library(openxlsx)

# ------------------------------------------------------------
# Name utilities
# ------------------------------------------------------------

#' Split a name into up to N components (space-delimited)
#'
#' @param name character(1). A single full name.
#' @param name_length integer(1). Max number of components to return.
#'
#' @return character vector of length `name_length` (padded with NA).
#' @examples
#' name_split("Ada Lovelace", 5)
name_split <- function(name, name_length) {
  # if name is missing or only whitespace, return all NA components
  if (is.na(name) || !nzchar(trimws(name))) {
    return(rep(NA_character_, name_length))
  }
  # trim leading/trailing spaces
  nm_trim <- stringr::str_trim(name)
  # split by one or more whitespace characters into a character vector
  parts   <- stringr::str_split(nm_trim, "\\s+", n = Inf, simplify = FALSE)[[1]]
  # count how many components we have
  len     <- length(parts)
  
  if (len <= name_length) {
    # if fewer than name_length parts, pad with NA up to name_length
    c(parts, rep(NA_character_, name_length - len))
  } else {
    # Keep the first name_length-1 parts; collapse the remainder into the last slot
    c(parts[1:(name_length - 1)], paste(parts[name_length:len], collapse = " "))
  }
}

#' Remove non-letters and lowercase (for rough name matching)
#'
#' @param x character vector.
#' @return character vector: only a-z, with spaces for removed chars.
remove_special_char <- function(x) {
  # convert all characters to lower case
  x <- tolower(x)
  # keep letters, replace everything else with space, then trim
  out <- gsub("[^a-z]", " ", x, perl = TRUE)
  # squash multiple spaces to one and trim
  trimws(gsub("\\s+", " ", out))
}

#' Split a name column into multiple `name_1...name_k` columns (vectorised)
#'
#' @param data data.frame/tibble.
#' @param name_var string. Column in `data` holding names.
#' @param name_length integer. Number of components to create.
#' @param clean_fun function. Applied before splitting (default `remove_special_char`).
#' @param split_fun function. The splitter (default `name_split`).
#' @param prefix string. Prefix for output columns (default "name_").
#' @param keep_clean logical. Keep intermediate `name_clean` column? (default TRUE)
#'
#' @return `data` with added `name_clean` and `prefix1..prefixN` columns.
split_names <- function(data,
                        name_var,
                        name_length,
                        clean_fun  = remove_special_char,
                        split_fun  = name_split,
                        prefix     = "name_",
                        keep_clean = TRUE) {
  # basic argument checks for structure and lengths
  stopifnot(is.data.frame(data),
            is.character(name_var), length(name_var) == 1,
            is.numeric(name_length), length(name_length) == 1, name_length >= 1)
  
  # temporary column name that won't clash with user columns
  tmp_col <- ".name_split_temp"
  
  out <- data |>
    # clean original name column (remove special chars, lower, etc.)
    dplyr::mutate(name_clean = clean_fun(.data[[name_var]])) |>
    # apply split_fun to each cleaned name, store list-column of vectors
    dplyr::mutate(!!tmp_col := lapply(name_clean, split_fun, name_length = name_length)) |>
    # expand list-column into separate columns tmp_col_1..tmp_col_k
    tidyr::unnest_wider(dplyr::all_of(tmp_col), names_sep = "_", strict = FALSE) |>
    # rename those temp columns to prefix1..prefixN
    dplyr::rename(!!!setNames(
      paste0(tmp_col, "_", seq_len(name_length)),
      paste0(prefix,  seq_len(name_length))
    )) |>
    # move name_x columns to sit immediately after name_clean
    dplyr::relocate(dplyr::starts_with(prefix), .after = name_clean)
  
  # optionally drop the intermediate name_clean
  if (!keep_clean) out <- dplyr::select(out, -name_clean)
  out
}

#' Shift tokens like "jr"/"jnr"/"junior"/"dr" out of name slots and left-pack
#'
#' @param df data.frame with columns listed in `cols`.
#' @param cols character. Name component columns to inspect (default name_1..name_5).
#' @param tokens character. Tokens to remove (case-insensitive).
#' @return df with same columns, tokens removed and components left-packed.
shift_out_junior <- function(df,
                             cols = c("name_1", "name_2", "name_3", "name_4", "name_5"),
                             tokens = c("jr", "jnr", "junior", "dr")) {
  # ensure specified columns exist in df
  stopifnot(all(cols %in% names(df)))
  
  # whole word, optional trailing dot, case-insensitive
  # e.g. matches "jr", "JR.", "Dr", etc.
  pattern <- paste0("(?i)^(", paste(tokens, collapse = "|"), ")\\.?$")
  
  df |>
    dplyr::mutate(
      # use pick() so we don't need the '.' placeholder
      .shifted = purrr::pmap(dplyr::pick(dplyr::all_of(cols)), function(...) {
        # combine the listed name columns into a character vector
        v <- as.character(c(...))
        # trim whitespace in each component
        v <- trimws(v)
        # keep non-missing, non-empty entries that do not match the token pattern
        keep <- !is.na(v) & nzchar(v) & !stringr::str_detect(v, pattern)
        # vector of cleaned name pieces (no jr/jnr/dr)
        v_clean <- v[keep]
        # pad with NA to original number of columns (left-packed)
        res <- c(v_clean, rep(NA_character_, length(cols) - length(v_clean)))
        # name the vector elements with original column names
        stats::setNames(res, cols)
      })
    ) |>
    # expand list-column into separate columns .shifted_name_1.. etc.
    tidyr::unnest_wider(.shifted, names_sep = "_") |>
    # drop original name_ columns
    dplyr::select(-dplyr::all_of(cols)) |>
    # rename .shifted_name_1 → name_1 etc.
    dplyr::rename_with(~ sub("^\\.shifted_", "", .x), dplyr::starts_with(".shifted_"))
}

# ------------------------------------------------------------
# DOB parsing
# ------------------------------------------------------------

#' Parse a DOB column into Date + day/month/year components
#'
#' Supports Excel serials (`dob_format = "numeric"`) or free-form tokens
#' described by an order string containing exactly one each of d/dd, m/mm,
#' and yy/yyyy (e.g., "dd/mm/yyyy", "yyyy-mm-dd", "m-d-yy").
#'
#' Two-digit years roll into 1900s/2000s using a "<= current YY → 2000s" rule.
#'
#' @param data data.frame
#' @param dob_var string. Column name in `data`.
#' @param dob_format string. "numeric" or pattern tokens (default "dd/mm/yyyy").
#' @param excel_origin string. Excel origin (default "1899-12-30").
#'
#' @return data with `dob` (Date) and `dob_day`, `dob_month`, `dob_year` numeric columns added.
parse_dob_columns <- function(data,
                              dob_var,
                              dob_format = "dd/mm/yyyy",
                              excel_origin = "1899-12-30") {
  # sanity checks on inputs
  stopifnot(is.data.frame(data),
            is.character(dob_var), length(dob_var) == 1,
            is.character(dob_format), length(dob_format) == 1)
  
  # if no dob column specified, return original data unchanged
  if (is.null(dob_var)) return(data)
  # dob column must exist in data
  if (!dob_var %in% names(data)) stop(paste("DOB column", dob_var, "not found in data."))
  
  # Case 1: Excel serials
  if (tolower(dob_format) == "numeric") {
    return(
      data |>
        dplyr::mutate(
          # convert numeric/character serial to Date using Excel origin
          dob = as.Date(suppressWarnings(as.numeric(.data[[dob_var]])), origin = excel_origin),
          # extract day/month/year components
          dob_day   = lubridate::day(dob),
          dob_month = lubridate::month(dob),
          dob_year  = lubridate::year(dob)
        )
    )
  }
  
  # Case 2: token format "d/m/y" variants
  # extract format tokens (dd,mm,yyyy) from the dob_format string
  tokens <- stringr::str_extract_all(dob_format, "(yyyy|yy|mm|m|dd|d)")[[1]]
  # require exactly one y-token, one m-token, one d-token
  if (length(tokens) != 3L ||
      !any(tokens %in% c("yyyy","yy")) ||
      !any(tokens %in% c("mm","m"))   ||
      !any(tokens %in% c("dd","d"))) {
    stop("dob_format must contain one each of day (d/dd), month (m/mm), and year (yy/yyyy).")
  }
  
  # temporary columns to hold separated components
  tmp_cols <- paste0("..dob_part", seq_along(tokens))
  out <- data |>
    # separate DOB string into up to 3 parts based on non-alphanumeric separators
    tidyr::separate(
      col = !!rlang::sym(dob_var),
      into = tmp_cols,
      sep  = "[^A-Za-z0-9]+",
      remove = FALSE,
      fill = "right",
      extra = "drop"
    )
  
  # map tokens (dd/mm/yy) to the temp column positions
  pos_map <- stats::setNames(tmp_cols, tokens)
  # helper to retrieve the correct temp column for a given token set
  pick <- function(keys) {
    k <- intersect(keys, names(pos_map))
    if (length(k) != 1L) stop("Ambiguous or missing token mapping in dob_format.")
    pos_map[[k]]
  }
  # pick which column holds day/month/year
  col_day   <- pick(c("dd","d"))
  col_month <- pick(c("mm","m"))
  col_year  <- pick(c("yyyy","yy"))
  
  # current 2-digit year (YY) for century-splitting logic
  current_yy <- as.integer(format(Sys.Date(), "%y"))
  
  out |>
    dplyr::mutate(
      # parse numeric day/month/year from the separated parts
      dob_day_raw   = suppressWarnings(as.numeric(.data[[col_day]])),
      dob_month_raw = suppressWarnings(as.numeric(.data[[col_month]])),
      dob_year_raw  = suppressWarnings(as.numeric(.data[[col_year]])),
      dob_year_txt  = trimws(.data[[col_year]])
    ) |>
    dplyr::mutate(
      # convert 2-digit years into 1900s/2000s based on current_yy
      dob_year = dplyr::case_when(
        !is.na(dob_year_raw) & nchar(dob_year_txt) <= 2 ~
          ifelse(dob_year_raw <= current_yy, 2000 + dob_year_raw, 1900 + dob_year_raw),
        TRUE ~ dob_year_raw
      ),
      dob_month = dob_month_raw,
      dob_day   = dob_day_raw
    ) |>
    dplyr::mutate(
      # guard against invalid day/month values
      dob_day   = dplyr::if_else(dob_day   < 1 | dob_day   > 31, NA_real_, dob_day),
      dob_month = dplyr::if_else(dob_month < 1 | dob_month > 12, NA_real_, dob_month)
    ) |>
    dplyr::mutate(
      # build a proper Date from cleaned year/month/day
      dob = suppressWarnings(lubridate::make_date(year = dob_year,
                                                  month = dob_month,
                                                  day = dob_day))
    ) |>
    # drop temporary parsing columns and raw components
    dplyr::select(-tidyselect::any_of(c(tmp_cols, "dob_day_raw", "dob_month_raw",
                                        "dob_year_raw", "dob_year_txt")))
}

# ------------------------------------------------------------
# Display / clerical review helpers
# ------------------------------------------------------------
#' Insert a blank row after each group to aid clerical review
#'
#' @param data data.frame/tibble
#' @param group_var string or character vector of columns to group by
#' @return data.frame with a blank row after each group. 
#'         All NAs become "" BUT date values remain intact.
add_blank_row_between_groups <- function(data, group_var) {
  
  # Split into groups
  groups <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::group_split()
  
  # Create blank row with correct types
  blank_row <- data[NA_integer_, ]
  
  # Tag the blank row (so we can handle it after binding)
  blank_row$.blank_flag <- TRUE
  data$.blank_flag <- FALSE
  
  # Bind groups + blank rows
  out <- purrr::map2(groups, seq_along(groups), function(g, i) {
    # ensure group rows are not flagged as blank
    g$.blank_flag <- FALSE
    if (i < length(groups)) {
      # for all but last group, append a blank row
      dplyr::bind_rows(g, blank_row)
    } else {
      # last group: just return the group
      g
    }
  }) |>
    dplyr::bind_rows()
  
  # --- Replace NAs with "" in NON-date columns ---
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        .cols = !where(is.Date) & !dplyr::matches("\\.blank_flag"),
        .fns  = ~ ifelse(is.na(.), "", .)
      )
    )
  
  # --- Fix Date columns: blank rows get "" instead of NA ---
  # identify which columns are Date class
  date_cols <- names(out)[sapply(out, inherits, what = "Date")]
  
  # for date columns, set blank-flagged rows to "" (for export)
  for (col in date_cols) {
    out[[col]][out$.blank_flag] <- ""   # blank row becomes ""
  }
  
  # helper to find date-like columns (Date/POSIX)
  get_date_like_vars <- function(df) {
    date_cols <- names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")))]
    return(date_cols)
  }
  
  # capture all date-like variables in the output
  date_vars = get_date_like_vars(out)
  
  # convert date-like columns to character and replace remaining NAs with ""
  for (col in date_vars) {
    out[[col]] <- as.character(out[[col]])
    out[[col]][is.na(out[[col]])] <- ""
  }
  
  # remove helper flag
  out$.blank_flag <- NULL
  
  out
}

#' Print or return a small dedup summary (fastLink group tallies expected)
#'
#' Expects columns: `duplicates` (per group size) and `rowid` (1 for group head).
#'
#' @param data data.frame with `duplicates` and `rowid`.
#' @param return_df logical. If TRUE, returns the summary data.frame.
#' @return (invisibly) NULL or data.frame if `return_df=TRUE`.
print_dedup_summary <- function(data, return_df = FALSE) {
  
  # total number of rows in the dataset
  num_df           <- nrow(data)
  # number of records that belong to duplicate groups (duplicates > 1)
  num_duplicates   <- data |>
    dplyr::filter(.data$duplicates > 1) |>
    nrow()
  # number of potential unique IDs (group head per dedupe.ids group)
  num_pot_unique_ids <- data |>
    dplyr::filter(.data$rowid == 1) |>
    nrow()
  # number of records that are in singleton groups (duplicates == 1)
  num_dedup <- data |>
    dplyr::filter(.data$duplicates == 1) |>
    nrow()
  
  # build a small summary table
  summary_df <- data.frame(
    text = c("Total number of records:",
             "Number of potential duplicate records:",
             "Number of unique records:",
             "Number of potentially unique IDs:"),
    records = c(num_df, num_duplicates, num_dedup, num_pot_unique_ids),
    stringsAsFactors = FALSE
  )
  # print summary to console
  print(summary_df)
  # optionally return the summary as a data.frame
  if (return_df) return(summary_df)
  invisible(NULL)
}

# ------------------------------------------------------------
# Main cleaning + self-dedup via fastLink
# ------------------------------------------------------------

#' Clean names/DOB/sex, create linking fields, and self-deduplicate with fastLink
#'
#' @param df data.frame or NULL (if reading from `filepath`)
#' @param filepath string or NULL (xlsx file)
#' @param name_var string. Name column or columns (if using `combine_name_vars`).
#' @param name_length integer. Number of name components to create.
#' @param combine_name_vars character or NULL. If set, concatenates these cols into `name`.
#' @param filter_conditions named list {var = c(values)} to filter rows.
#' @param dob_var string or NULL. DOB column name (raw).
#' @param dob_format string. See `parse_dob_columns()`.
#' @param sex_var string or NULL. Sex/sex-code column.
#' @param sex_key named character vector mapping raw codes to "Male"/"Female".
#' @param linking_var character or NULL. Extra vars to include for linking.
#' @param stringdist string or character vec. Name fields to fuzzy-match (must be in dedup set).
#' @param address_field string or NULL. Address field name for fastLink.
#' @param dedup_var character or NULL. Variables used by fastLink for deduplication.
#' @param output_file_path/output_file_name optional CSV outputs.
#' @param group_id_var string. Column name for the dedupe group id.
#'
#' @return list with:
#'   - dedup_var
#'   - cleaned_all: all rows with group id, counters
#'   - deduplicated_data: one row per group
#'   - duplicated_data: only groups with duplicates
#'   - fastlink_output: full fastLink object
data_clean_fn <- function(df = NULL,
                          filepath = NULL,
                          name_var = "name",
                          name_length = 5,
                          combine_name_vars = NULL,   
                          filter_conditions = NULL,   # (still expects cleaned names)
                          dob_var = NULL,             
                          dob_format = "dd/mm/yyyy",
                          sex_var = NULL,            
                          sex_key = c("M" = "Male", "F" = "Female", "1" = "Male", "2" = "Female"),
                          linking_var = NULL,        
                          stringdist = c("name_clean"),
                          address_field = NULL,       
                          dedup_var = NULL,        
                          output_file_path = NULL,
                          output_file_name = NULL,
                          group_id_var = "group_id") {
  
  # choose between data supplied directly vs read from file
  if (!is.null(df)) {
    data <- df
  } else if (!is.null(filepath)) {
    data <- readxl::read_xlsx(filepath)
  } else {
    stop("Provide either `df` or `filepath`.")
  }
  
  # store original column names and their cleaned versions (before cleaning)
  orig_names <- names(data)
  name_map <- setNames(janitor::make_clean_names(orig_names), orig_names)
  
  data <- data |>
    # standardise column names to snake_case etc.
    janitor::clean_names() |>
    # coerce all columns to character for consistent handling
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    # create a stable row id (original row index)
    dplyr::mutate(.rowid = dplyr::row_number())
  
  # Helpers: map originals -> cleaned
  map_to_clean <- function(v) {
    # if already present as cleaned name, keep as is
    if (v %in% names(data)) v
    # if original name appears in name_map, map to cleaned version
    else if (v %in% names(name_map)) unname(name_map[[v]])
    # else return NA (not found)
    else NA_character_
  }
  # map a vector of user-specified variables using map_to_clean, with validation
  map_set_to_clean <- function(vec, arg_name) {
    mapped <- unique(vapply(vec, map_to_clean, FUN.VALUE = character(1)))
    if (anyNA(mapped) || !all(mapped %in% names(data))) {
      bad <- vec[is.na(mapped) | !(mapped %in% names(data))]
      stop("`", arg_name, "` not found (original or cleaned): ", paste(bad, collapse = ", "))
    }
    mapped
  }
  
  # Optional filters (kept as cleaned-name inputs)
  if (!is.null(filter_conditions)) {
    # get filter variable names
    filt_vars <- names(filter_conditions)
    # ensure all filter vars exist in data
    if (!all(filt_vars %in% names(data))) {
      stop("Filter var(s) not found: ", paste(setdiff(filt_vars, names(data)), collapse = ", "))
    }
    # iteratively filter data based on each var/value set
    for (v in filt_vars) {
      data <- dplyr::filter(data, .data[[v]] %in% filter_conditions[[v]])
    }
  }
  
  # Combine name parts
  if (!is.null(combine_name_vars)) {
    # map supplied name columns to cleaned names
    combine_name_vars_clean <- map_set_to_clean(combine_name_vars, "combine_name_vars")
    data <- data |>
      # work rowwise so we can paste across columns
      dplyr::rowwise() |>
      dplyr::mutate(
        # squish multiple spaces and paste multiple name columns into a single name
        name = stringr::str_squish(
          paste(c_across(dplyr::all_of(combine_name_vars_clean)), collapse = " ")
        )
      ) |>
      dplyr::ungroup()
    # set name_var to the newly created name column
    name_var <- "name"
  }
  # ensure name_var is present after any modifications
  if (!name_var %in% names(data)) stop("`name_var` not found.")
  
  # Name cleaning/splitting
  # create name_clean and name_1..name_k columns
  data <- split_names(data, name_var, name_length)
  
  # DOB parsing
  if (!is.null(dob_var)) {
    # map DOB column(s) to cleaned names
    dob_var_clean <- map_set_to_clean(dob_var, "dob_var")
    # parse DOB into date + day/month/year components
    data <- parse_dob_columns(data, dob_var_clean, dob_format)
  }
  
  # Sex standardisation
  if (!is.null(sex_var)) {
    # map sex column to cleaned name; use only first
    sex_var_clean <- map_set_to_clean(sex_var, "sex_var")[1]
    # create canonical sex column from mapped variable
    data <- dplyr::mutate(data, sex = .data[[sex_var_clean]])
    if (!is.null(sex_key)) {
      # mapping must be a named character vector
      if (!is.character(sex_key) || is.null(names(sex_key)))
        stop("`sex_key` must be a named character vector.")
      # recode sex values using sex_key mapping
      data <- dplyr::mutate(data, sex = dplyr::recode(sex, !!!sex_key))
    }
    # restrict to standard Male/Female labels; otherwise set to NA
    data <- dplyr::mutate(data, sex = ifelse(sex %in% c("Male","Female"), sex, NA_character_))
  }
  
  # Map user-supplied sets
  # additional variables to carry through in linking
  user_linking_clean <- if (!is.null(linking_var)) map_set_to_clean(linking_var, "linking_var") else NULL
  # variables to use as dedup keys
  user_dedup_clean   <- if (!is.null(dedup_var))   map_set_to_clean(dedup_var,   "dedup_var")   else NULL
  # optional address field used in fastLink
  address_field_clean <- if (!is.null(address_field)) map_set_to_clean(address_field, "address_field") else NULL
  
  # Build final sets
  # base linking variables including rowid, names, dob, sex, and any extras
  linking_var <- unique(c(".rowid",
                          "name_clean", paste0("name_", 1:name_length),
                          "dob","dob_day","dob_month","dob_year",
                          "sex", user_linking_clean))
  # dedup variables are a subset of name_clean/dob/sex plus any extras
  dedup_var <- unique(c("name_clean","dob","sex", user_dedup_clean))
  
  # create a data frame of linking variables and a group id (group_id_var)
  data_linking <- dplyr::select(data, dplyr::all_of(linking_var)) %>%
    dplyr::mutate(!!rlang::sym(group_id_var) := .rowid)
  
  # Self-link dedup
  # run fastLink with A=B to find duplicates within the same file
  fl <- fastLink::fastLink(
    dfA = data_linking,
    dfB = data_linking,
    varnames = dedup_var,
    stringdist.match = stringdist,
    w.lambda = 0.5,
    address.field = address_field_clean,  # <<< mapped here
    return.df = TRUE
  )
  
  # extract matched pairs and summarise duplicates at dedupe group level
  dedup_df <- fastLink::getMatches(dfA = data_linking, dfB = data_linking, fl.out = fl) |>
    # ensure group id column is integer
    dplyr::mutate(!!rlang::sym(group_id_var) := as.integer(.data[[group_id_var]])) |>
    # group by dedupe.ids (fastLink group label)
    dplyr::group_by(dedupe.ids) |>
    dplyr::mutate(
      # count number of records per dedupe group
      duplicates = dplyr::n(),
      # assign row number within each group (1=head)
      rowid = dplyr::row_number()
    ) |>
    dplyr::ungroup() |>
    # order by group id for readability
    dplyr::arrange(!!rlang::sym(group_id_var))
  
  # subset of mapping columns from dedup result
  id_map <- dplyr::select(dedup_df, .rowid, !!rlang::sym(group_id_var), duplicates, rowid, dedupe.ids)
  
  # join id_map back to full cleaned data to attach duplicates metadata
  cleaned_all <- data |>
    dplyr::left_join(id_map, by = ".rowid") |>
    dplyr::mutate(
      # singleton records get duplicates=1 by default
      duplicates = dplyr::coalesce(duplicates, 1L),
      # rowid defaults to 1 for non-matched cases
      rowid      = dplyr::coalesce(rowid, 1L)) |>
    # order by group id, then rowid, then original .rowid
    dplyr::arrange(!!rlang::sym(group_id_var), rowid, .rowid)
  
  # for final deduplicated_data, pick one representative per group
  deduplicated_data <- cleaned_all |>
    # keep groups marked as duplicates == 1L (one per dedupe.ids) or treat grouped by group_id_var
    dplyr::filter(.data$duplicates == 1L) |>
    dplyr::group_by(!!rlang::sym(group_id_var)) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()
  
  # duplicated_data retains groups where duplicates > 1 for clerical review
  duplicated_data <- cleaned_all |>
    dplyr::filter(.data$duplicates > 1L) |>
    dplyr::arrange(!!rlang::sym(group_id_var), rowid)
  
  # Optional CSV outputs
  if (!is.null(output_file_path) && !is.null(output_file_name)) {
    # write full cleaned dataset with dedupe metadata
    utils::write.csv(
      cleaned_all,
      file = file.path(output_file_path, paste0("cleaned_all ", output_file_name)),
      row.names = FALSE
    )
    # write deduplicated subset (one row per dedupe group) with linking vars
    deduplicated_data %>%
      dplyr::select(!!rlang::sym(group_id_var), dplyr::all_of(linking_var)) %>%
      dplyr::select(-.rowid) %>%
      utils::write.csv(
        file = file.path(output_file_path, paste0("dedup ", output_file_name)),
        row.names = FALSE)
    
    # prepare duplicates-only file for clerical review
    duplicated_data |>
      # blank column for human decision entry
      dplyr::mutate(clerical_review = "") |>
      dplyr::select(
        clerical_review,
        dplyr::all_of(linking_var),
        !!rlang::sym(group_id_var), duplicates, rowid, dedupe.ids) |>
      # add blank row between groups for readability in Excel
      add_blank_row_between_groups("dedupe.ids") |>
      # drop internal columns not needed in review
      dplyr::select(-.rowid, -dedupe.ids) |>
      utils::write.csv(
        file = file.path(output_file_path, paste0("duplicates ", output_file_name)),
        row.names = FALSE)
  }
  
  # return all key outputs as a list for downstream use
  list(
    dedup_var          = dedup_var,
    cleaned_all        = cleaned_all,
    deduplicated_data  = deduplicated_data,
    duplicated_data    = duplicated_data,
    fastlink_output    = fl
  )
}

# ------------------------------------------------------------
# IO helpers for pre-processed files (kept minimal/robust)
# ------------------------------------------------------------

#' Load and combine a deduped CSV and its corresponding labelled-duplicate CSV
#'
#' Safely loads two CSV files:
#' (1) the deduplicated dataset ("dedup {label} data.csv")
#' (2) the reviewed duplicate dataset ("duplicates {label}_labelled.csv").
#'
#' Keeps only rows where `clerical_review == 1`, drops selected columns,
#' and returns a combined data frame. If either file ends up with zero rows,
#' the function still returns a valid data frame.
#'
#' @param base_path string. Directory containing the CSV files.
#' @param label string. Base filename label.
#'
#' @return A combined data.frame of deduped rows + reviewed duplicates.
#'
load_and_process_data <- function(base_path, label) {
  
  # ---- Load main deduped file ----
  # construct path to deduplicated data file
  main_file <- file.path(base_path, paste0("dedup ", label, ".csv"))
  # read deduped CSV
  df_main <- utils::read.csv(main_file, check.names = FALSE)
  
  # Coerce DOB components to numeric
  if (nrow(df_main) > 0) {
    df_main <- df_main |>
      dplyr::mutate(
        # convert dob_* fields to numeric if possible
        dob_day   = suppressWarnings(as.double(.data$dob_day)),
        dob_month = suppressWarnings(as.double(.data$dob_month)),
        dob_year  = suppressWarnings(as.double(.data$dob_year)),
        # rebuild dob as "YYYY-MM-DD" string (no conversion to Date here)
        dob = paste0(dob_year,"-",dob_month,"-",dob_day)
      )
  }
  
  # ---- Load duplicate-labelled file ----
  # construct path to labelled duplicates file
  dup_file <- file.path(base_path, paste0("duplicates ", label, "_labelled.csv"))
  # read duplicates CSV
  df_dup <- utils::read.csv(dup_file, check.names = FALSE)
  
  # keep only records where clerical review accepted the match (clerical_review == 1)
  df_dup <- df_dup %>%
    dplyr::filter(.data$clerical_review == 1) %>%
    # reconstruct dob string for consistency with df_main
    dplyr::mutate(dob = paste0(dob_year,"-",dob_month,"-",dob_day))
  
  # ---- Filter to clerical-reviewed matches ----
  if (nrow(df_dup) > 0) {
    
    # Remove columns if they exist
    # drop internal review columns that are no longer needed
    remove_cols <- intersect(c("duplicates", "rowid_within_group", "clerical_review"), names(df_dup))
    if (length(remove_cols) > 0) {
      df_dup <- df_dup |> dplyr::select(-tidyselect::all_of(remove_cols))
    }
    
    # append reviewed duplicates to main deduped dataset
    df_main = dplyr::bind_rows(df_main, df_dup)
    
  }
  
  # ---- Combine and return ----
  # bind_rows() handles empty data frames automatically
  return(df_main)
}

#' Load dataset from object or fallback to file
#'
#' @param df_object data.frame or NULL.
#' @param df_path string or NULL.
#' @param df_file string or NULL.
#' @param default data.frame or NULL.
#' @param df_name string (used in error messages).
#' @return data.frame
load_dataset_if_needed <- function(df_object = NULL, df_path = NULL, df_file = NULL, default = NULL, df_name = "df") {
  # priority 1: if both path and file provided, load via load_and_process_data()
  if (!is.null(df_path) && !is.null(df_file)) {
    return(load_and_process_data(df_path, df_file))
  } else if (!is.null(df_object)) {
    # priority 2: use the in-memory data object
    return(df_object)
  } else if (!is.null(default)) {
    # priority 3: fall back to a default data frame if provided
    return(default)
  } else {
    # if none provided, signal error
    stop(paste("You must supply either a data object or file path for", df_name))
  }
}

# ------------------------------------------------------------
# SQL upsert helpers
# ------------------------------------------------------------

#' Insert new key rows into a target SQL table
#'
#' @param con DBI connection.
#' @param new_data data.frame with `key_cols`.
#' @param table DBI::Id or string "schema.table" / "catalog.schema.table".
#' @param key_cols character. Columns that define uniqueness.
#' @param extra_cols_sql named character of SQL literals (unbound extras).
#' @return invisible integer: number of inserted rows.
upload_new_rows <- function(
    con,
    new_data,
    table = "master.dbo.spr",
    key_cols,
    extra_cols_sql = c(departure_id="NULL", birth_id="NULL", death_id="NULL")
) {
  # confirm we have a data.frame
  stopifnot(is.data.frame(new_data))
  # ensure all key_cols exist in new_data
  if (!all(key_cols %in% names(new_data))) {
    stop("Missing key_cols in new_data: ", paste(setdiff(key_cols, names(new_data)), collapse = ", "))
  }
  
  # normalise table reference: DBI::Id or schema-qualified string
  table_sql <- if (inherits(table, "Id")) as.character(DBI::dbQuoteIdentifier(con, table))
  else if (is.character(table) && length(table) == 1) table
  else stop("`table` must be DBI::Id(...) or a single schema-qualified string.")
  
  # quote key column names for SQL
  key_cols_q <- vapply(key_cols, function(x) as.character(DBI::dbQuoteIdentifier(con, x)), character(1))
  
  # query to fetch current key combinations from target table
  existing_q <- paste0("SELECT ", paste(key_cols_q, collapse = ", "), " FROM ", table_sql, ";")
  existing_keys <- DBI::dbGetQuery(con, existing_q)
  
  # keep only rows in new_data whose keys do not appear in existing_keys
  to_insert <- dplyr::anti_join(new_data, existing_keys, by = key_cols)
  if (nrow(to_insert) == 0) {
    message("No new rows to insert.")
    return(invisible(0L))
  }
  
  # extra columns defined as SQL literals (e.g. NULL)
  extra_cols_q  <- if (length(extra_cols_sql)) names(extra_cols_sql) else character(0)
  extra_cols_q  <- vapply(extra_cols_q, function(x) as.character(DBI::dbQuoteIdentifier(con, x)), character(1))
  # all columns to be inserted (keys + extras)
  insert_cols_q <- c(key_cols_q, extra_cols_q)
  
  # build parameter placeholders for key values (one "?" per key column)
  placeholders  <- paste(rep("?", length(key_cols)), collapse = ", ")
  # build extras clause (SQL literals, no binding)
  extras_clause <- if (length(extra_cols_sql)) paste(",", paste(extra_cols_sql, collapse = ", ")) else ""
  
  # assemble full INSERT statement
  insert_sql <- paste0(
    "INSERT INTO ", table_sql, " (", paste(insert_cols_q, collapse = ", "), ") ",
    "VALUES (", placeholders, extras_clause, ");"
  )
  
  message("Inserting ", nrow(to_insert), " new row(s) into ", table_sql, " ...")
  
  # prepare statement for multiple row bind
  stmt <- DBI::dbSendStatement(con, insert_sql)
  # ensure statement is cleared even if error occurs
  on.exit(try(DBI::dbClearResult(stmt), silent = TRUE), add = TRUE)
  
  # bind just the key columns as parameters
  DBI::dbBind(stmt, to_insert[, key_cols, drop = FALSE])
  DBI::dbClearResult(stmt)
  
  invisible(nrow(to_insert))
}

#' UPDATE existing rows by matching on join keys (NULL-safe), with optional txn
#'
#' @param con DBI connection.
#' @param new_data data.frame providing join and update columns.
#' @param table DBI::Id or string.
#' @param join_cols character. Key columns to match on (not updated).
#' @param update_cols character. Columns to update (cannot overlap with `join_cols`).
#' @param use_transaction logical. Wrap in a transaction if not already inside one.
#'
#' @return invisible integer: rows updated.
update_existing_rows <- function(
    con,
    new_data,
    table        = "master.dbo.spr",
    join_cols,
    update_cols,
    use_transaction = TRUE
) {
  # require data.frame and non-empty join/update columns
  stopifnot(is.data.frame(new_data))
  if (missing(join_cols) || !length(join_cols))  stop("Provide join_cols.")
  if (!all(join_cols %in% names(new_data))) stop("Missing join_cols in new_data: ",
                                                 paste(setdiff(join_cols, names(new_data)), collapse=", "))
  if (missing(update_cols) || !length(update_cols)) stop("Provide update_cols present in new_data.")
  if (!all(update_cols %in% names(new_data))) stop("Missing update_cols in new_data: ",
                                                   paste(setdiff(update_cols, names(new_data)), collapse=", "))
  # join_cols and update_cols must be disjoint
  if (length(intersect(join_cols, update_cols)) > 0)
    stop("update_cols must not include join_cols; keys cannot be updated.")
  
  # normalise table identifier
  table_sql <- if (inherits(table, "Id")) as.character(DBI::dbQuoteIdentifier(con, table))
  else if (is.character(table) && length(table) == 1) table
  else stop("`table` must be DBI::Id(...) or a single character like 'dbo.spr'.")
  
  # inspect existing table schema (no rows) to confirm column presence
  tbl_cols <- names(DBI::dbGetQuery(con, paste0("SELECT TOP 0 * FROM ", table_sql, ";")))
  if (!all(join_cols %in% tbl_cols))   stop("Some join_cols do not exist in target table: ",
                                            paste(setdiff(join_cols, tbl_cols), collapse=", "))
  if (!all(update_cols %in% tbl_cols)) stop("Some update_cols do not exist in target table: ",
                                            paste(setdiff(update_cols, tbl_cols), collapse=", "))
  
  # combine join and update columns in desired order
  all_cols <- c(join_cols, update_cols)
  # build staging data frame with only relevant columns
  stg_df <- new_data |>
    dplyr::select(dplyr::all_of(all_cols)) |>
    # coerce to integer (BIGINT) to match staging table definition
    dplyr::mutate(dplyr::across(dplyr::all_of(all_cols), ~ suppressWarnings(as.integer(.)))) |>
    # drop potential duplicates on join_cols
    dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)), .keep_all = TRUE)
  
  # if nothing left to update, exit early
  if (nrow(stg_df) == 0) {
    message("No valid rows to update after coercion / key filtering.")
    return(invisible(0L))
  }
  
  # quote join and update columns for SQL
  q_join <- vapply(join_cols,  function(x) as.character(DBI::dbQuoteIdentifier(con, x)), character(1))
  q_upd  <- vapply(update_cols,function(x) as.character(DBI::dbQuoteIdentifier(con, x)), character(1))
  q_all  <- c(q_join, q_upd)
  
  # SQL to manage temp staging table
  drop_stg_sql   <- "IF OBJECT_ID('tempdb..#stg') IS NOT NULL DROP TABLE #stg;"
  # define staging table columns as BIGINT NULL
  col_defs_sql   <- paste0(q_all, " BIGINT NULL", collapse = ", ")
  # create temporary staging table
  create_stg_sql <- paste0("CREATE TABLE #stg (", col_defs_sql, ");")
  # columns to insert into staging table
  insert_cols_sql<- paste(q_all, collapse = ", ")
  # placeholders for parameterised insert into staging table
  placeholders   <- paste(rep("?", length(all_cols)), collapse = ", ")
  
  # NULL-safe equality for join predicate: equality or both NULL
  nullsafe_pred <- function(col) paste0("(t.", col, " = s.", col, " OR (t.", col, " IS NULL AND s.", col, " IS NULL))")
  # ON clause for multi-column join
  on_clause  <- paste(vapply(q_join, nullsafe_pred, character(1)), collapse = " AND ")
  # SET clause mapping target columns to staging columns
  set_clause <- paste(paste0("t.", q_upd, " = s.", q_upd), collapse = ", ")
  
  # Track whether we started a transaction here
  started_txn <- FALSE
  if (use_transaction) {
    # check if a transaction is already in progress
    trn <- tryCatch(DBI::dbGetQuery(con, "SELECT @@TRANCOUNT AS tran_count;")$tran_count[1], error = function(e) 0L)
    if (is.na(trn) || trn == 0L) {
      # start a new transaction if none open
      DBI::dbExecute(con, "BEGIN TRAN;")
      started_txn <- TRUE
    }
  }
  # ensure we roll back if function exits unexpectedly after we started a txn
  on.exit({ if (use_transaction && started_txn) try(DBI::dbExecute(con, "ROLLBACK TRAN;"), silent = TRUE) }, add = TRUE)
  
  # ensure temp table is dropped even if an error occurs
  on.exit(try(DBI::dbExecute(con, "IF OBJECT_ID('tempdb..#stg') IS NOT NULL DROP TABLE #stg;"), silent = TRUE), add = TRUE)
  # drop any existing staging table instance
  DBI::dbExecute(con, drop_stg_sql)
  # create fresh staging table
  DBI::dbExecute(con, create_stg_sql)
  
  # prepare insert statement for staging table
  stmt_ins <- DBI::dbSendStatement(con, paste0("INSERT INTO #stg (", insert_cols_sql, ") VALUES (", placeholders, ");"))
  # clear statement handle on exit
  on.exit(try(DBI::dbClearResult(stmt_ins), silent = TRUE), add = TRUE)
  # bind all rows of stg_df into staging table
  DBI::dbBind(stmt_ins, stg_df[, all_cols, drop = FALSE])
  DBI::dbClearResult(stmt_ins)
  
  # perform NULL-safe update from staging table to target table
  rows_updated <- DBI::dbExecute(con, paste0(
    "UPDATE t SET ", set_clause,
    " FROM ", table_sql, " AS t",
    " INNER JOIN #stg AS s ON ", on_clause, ";"
  ))
  
  # if we started a transaction here, commit after successful update
  if (use_transaction && started_txn) DBI::dbExecute(con, "COMMIT TRAN;")
  message("Rows updated: ", rows_updated)
  invisible(rows_updated)
}

# ------------------------------------------------------------
# fastLink linking helpers (A vs B) + clerical sampling
# ------------------------------------------------------------

#' Convert columns to numeric safely
#'
#' @param df data.frame
#' @param var_names character vector of columns to coerce
#' @return data.frame
convert_to_numeric <- function(df, var_names) {
  df |>
    # suppress conversion warnings and coerce selected columns to numeric
    dplyr::mutate(dplyr::across(dplyr::all_of(var_names), ~ suppressWarnings(as.numeric(.))))
}

#' Lowercase selected string variables
#'
#' Converts all values in the columns listed in `stringdist_vars` to lower case.
#' Character columns are lowercased directly; factor columns have their levels
#' lowercased while preserving the factor type. Columns not present in `df` are
#' safely ignored (via `dplyr::any_of()`); non-string columns are left unchanged.
#'
#' @param df A data frame or tibble.
#' @param stringdist_vars Character vector of column names to lowercase.
#'
#' @return A data frame/tibble like `df` with the specified columns converted to
#' lower case (characters lowercased; factor levels lowercased). Other columns
#' are unchanged.
#'
#' @examples
#' # df <- tibble::tibble(name_clean = c("ALICE","Bob"),
#' #                      suburb = factor(c("MELBOURNE","SYDNEY")),
#' #                      age = c(30, 25))
#' # lowercase_string_vars(df, c("name_clean","suburb"))
#'
lowercase_string_vars <- function(df, stringdist_vars) {
  # basic checks
  stopifnot(is.data.frame(df))
  stopifnot(is.character(stringdist_vars))
  
  dplyr::mutate(
    df,
    dplyr::across(
      dplyr::any_of(stringdist_vars),
      ~ {
        if (is.factor(.x)) {
          # lower-case factor levels, keep factor type
          lv <- levels(.x)
          levels(.x) <- tolower(lv)
          .x
        } else if (is.character(.x)) {
          # for character, just lower-case strings
          tolower(.x)
        } else {
          # non-string columns are returned unchanged
          .x
        }
      }
    )
  )
}

#' Run fastLink between dfA and dfB and return cleaned one-to-one id mapping
#'
#' @param linking_vars character. Variables to pass to fastLink `varnames`.
#' @param df_a,df_b data.frames (or use *_path + *_file to load via `load_and_process_data`).
#' @param df_a_path,df_a_file,df_b_path,df_b_file strings for lazy loading.
#' @param numeric_vars character. Names to match as numeric.
#' @param stringdist_vars character. Names for stringdistance and partial matches.
#' @param rowid_a,rowid_b strings. Column names containing stable row ids in dfA/dfB.
#' @return list(link_result, id_df, linkage_summary)
run_fastlink <- function(linking_vars,
                         df_a = NULL,
                         df_b = NULL,
                         df_a_path = NULL,
                         df_a_file = NULL,
                         df_b_path = NULL,
                         df_b_file = NULL,
                         numeric_vars = c("dob_day","dob_month","dob_year"),
                         stringdist_vars = c("name_clean","name_1","name_2","name_3","name_4","name_5"),
                         rowid_a,
                         rowid_b) {
  
  # load dfA from object or CSVs if not provided
  dfA <- load_dataset_if_needed(df_a, df_a_path, df_a_file, df_name = "dfA")
  # load dfB similarly
  dfB <- load_dataset_if_needed(df_b, df_b_path, df_b_file, df_name = "dfB")
  
  # ensure numeric matching columns are numeric
  dfA <- convert_to_numeric(dfA, numeric_vars)
  dfB <- convert_to_numeric(dfB, numeric_vars)
  
  # ensure stringdistance fields are lower-cased consistently
  dfA <- lowercase_string_vars(dfA, stringdist_vars)
  dfB <- lowercase_string_vars(dfB, stringdist_vars)
  
  # run fastLink with numeric + stringdistance matches
  link_result <- fastLink::fastLink(
    dfA = dfA,
    dfB = dfB,
    varnames        = linking_vars,    
    numeric.match   = numeric_vars,
    partial.match   = stringdist_vars,
    stringdist.match= stringdist_vars,
    threshold.match = 0.01
  )
  
  # build a data.frame of matched row indices and posterior probabilities
  id_df <- data.frame(
    rowid.a   = link_result$matches$inds.a,
    rowid.b   = link_result$matches$inds.b,
    posterior = link_result$posterior
  ) |>
    dplyr::mutate(
      # coerce row indices to numeric
      rowid.a = as.numeric(.data$rowid.a),
      rowid.b = as.numeric(.data$rowid.b)
    ) |>
    # drop pairs where a-side index appears more than once (enforce 1-to-1)
    dplyr::group_by(.data$rowid.a) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::ungroup() |>
    # then drop pairs where b-side index appears more than once
    dplyr::group_by(.data$rowid.b) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$n)
 
  # build mapping from rowid.a indices to actual IDs in dfA (rowid_a column)
  dfA_rowid <- dfA |>
    tibble::rowid_to_column("rowid.a") |>
    dplyr::select(.data$rowid.a, !!rlang::sym(rowid_a)) |>
    dplyr::filter(.data$rowid.a %in% id_df$rowid.a)
  
  # build mapping from rowid.b indices to actual IDs in dfB (rowid_b column)
  dfB_rowid <- dfB |>
    tibble::rowid_to_column("rowid.b") |>
    dplyr::select(.data$rowid.b, !!rlang::sym(rowid_b)) |>
    dplyr::filter(.data$rowid.b %in% id_df$rowid.b)
  
  # join row indices back to actual IDs and keep only the two key IDs + posterior
  id_df_joined <- id_df |>
    dplyr::left_join(dfA_rowid, by = "rowid.a") |>
    dplyr::left_join(dfB_rowid, by = "rowid.b") |>
    dplyr::select(!!rlang::sym(rowid_a), !!rlang::sym(rowid_b), .data$posterior)
  
  # return fastLink fit, cleaned id mapping, and threshold summary
  list(
    link_result      = link_result,
    id_df            = id_df_joined,
    linkage_summary  = summary(link_result, thresholds = c(0.9, 0.8, 0.7, 0.6, 0.5))
  )
}

# ------------------------------------------------------------
# Clerical-review support: sampling, reshape, join
# ------------------------------------------------------------

#' Sample an equal proportion across posterior bins for clerical review
#'
#' @param df data.frame containing `posterior.a` (or customise before calling).
#' @param p numeric in (0,1]. Proportion per bin.
#' @param n_bins integer. Number of equal-width bins in [0,1].
#' @param seed integer or NULL.
#' @return sampled tibble.
sample_equal_posterior <- function(df, p = 0.3, n_bins = 10, seed = 123) {
  # set seed for reproducible sampling if provided
  if (!is.null(seed)) set.seed(seed)
  df |>
    # assign each row to a posterior probability bin
    dplyr::mutate(bin = cut(.data$posterior.a,
                            breaks = seq(0, 1, length.out = n_bins + 1),
                            include.lowest = TRUE)) |>
    # group by bin
    dplyr::group_by(.data$bin) |>
    # sample the same proportion p from each bin
    dplyr::slice_sample(prop = p) |>
    dplyr::ungroup() |>
    # drop temporary bin column
    dplyr::select(-.data$bin)
}

#' Reshape A/B rows into blocks for clerical review; optional Excel with highlights
#'
#' @param df linked A/B combined rows (see `link_join_and_reshape()`).
#' @param source_label named chr c(data_source_a=..., data_source_b=...).
#' @param generate_xlsx logical.
#' @param output_path string filename.
#' @param sample_fraction numeric in (0,1]; passed to `sample_equal_posterior()`.
#' @return visible (sorted) data.frame with columns for review.
reshape_xy_to_end_example <- function(df,
                                      source_label = NULL,
                                      generate_xlsx = TRUE,
                                      output_path = "end_example.xlsx",
                                      sample_fraction = 0.3) {
  
  # identify column names belonging to A side (ending in .a)
  a_cols <- names(df) |> stringr::str_subset("\\.a$")
  # identify column names belonging to B side (ending in .b)
  b_cols <- names(df) |> stringr::str_subset("\\.b$")
  
  # derive "stem" variables (before .a/.b suffix)
  stems_a <- stringr::str_remove(a_cols, "\\.a$")
  stems_b <- stringr::str_remove(b_cols, "\\.b$")
  # intersection of stems that exist for both A and B
  stems   <- intersect(stems_a, stems_b)
  # remove empty stem name or any "rowid" pseudo-variable
  stems   <- stems[!(stems %in% c("", "rowid"))]
  if (length(stems) == 0) stop("No valid .a/.b variable pairs found.")
  
  # draw a proportional sample across posterior bins for review
  df <- df |> sample_equal_posterior(p = sample_fraction, n_bins = 10)
  
  # columns to show first in clerical review sheet
  leading_col <- c("clerical_review", "rowid")
  # final column order: clerical_review, rowid, stems..., data_source
  out_cols    <- c(leading_col, stems, "data_source")
  
  # build a three-row block (A, B, blank) for each candidate pair
  output_df <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    
    # extract A-side row as a single-row tibble with stem names
    row_a <- df |> dplyr::slice(i) |> dplyr::select(dplyr::all_of(paste0(stems, ".a")), -dplyr::all_of("rowid.a"))
    names(row_a) <- stems
    # extract B-side row similarly
    row_b <- df |> dplyr::slice(i) |> dplyr::select(dplyr::all_of(paste0(stems, ".b")), -dplyr::all_of("rowid.b"))
    names(row_b) <- stems
    
    # original rowid from dfA and dfB
    row_id_a <- df |> dplyr::slice(i) |> dplyr::pull(.data$rowid.a)
    row_id_b <- df |> dplyr::slice(i) |> dplyr::pull(.data$rowid.b)
    
    # use posterior value (if present) to build sorting key for block
    posterior_a <- suppressWarnings(as.numeric(row_a[["posterior"]][[1]]))
    posterior_b <- suppressWarnings(as.numeric(row_b[["posterior"]][[1]]))
    vals       <- c(posterior_a, posterior_b)
    # block_key = max posterior within the pair (for later ordering)
    block_key  <- if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
    
    # combine A row with clerical_review column (blank) and data source label
    a_row <- tibble::tibble(clerical_review = NA, rowid = row_id_a) |>
      dplyr::bind_cols(row_a) |>
      dplyr::mutate(data_source = source_label[["data_source_a"]],
                    .block_id = i, .block_order = 1L, .key = block_key)
    
    # combine B row similarly with its data source label
    b_row <- tibble::tibble(clerical_review = NA, rowid = row_id_b) |>
      dplyr::bind_cols(row_b) |>
      dplyr::mutate(data_source = source_label[["data_source_b"]],
                    .block_id = i, .block_order = 2L, .key = block_key)
    
    # add a blank separator row between pairs (with NA for numeric and "" later)
    blank_row <- tibble::as_tibble_row(stats::setNames(rep(NA, length(out_cols)), out_cols)) |>
      dplyr::mutate(.block_id = i, .block_order = 3L, .key = block_key)
    
    # stack A row, B row, and blank row
    dplyr::bind_rows(a_row, b_row, blank_row)
  }) |>
    # sort blocks by posterior key, then by block id and order within block
    dplyr::arrange(.data$.key, .data$.block_id, .data$.block_order)
  
  # columns that are visible in clerical review sheet
  output_visible <- dplyr::select(output_df, tidyselect::any_of(out_cols))
  
  # optionally write to Excel with highlighting
  if (isTRUE(generate_xlsx)) {
    wb <- openxlsx::createWorkbook()
    # add a single worksheet named "output"
    openxlsx::addWorksheet(wb, "output")
    # write data to the sheet
    openxlsx::writeData(wb, "output", output_visible)
    
    # define yellow cell style for highlighting matching values
    yellow <- openxlsx::createStyle(fgFill = "#FFFF00")
    n_cols <- ncol(output_visible); n_rows <- nrow(output_visible)
    # iterate over each column to find and highlight matching pairs
    for (j in seq_len(n_cols)) {
      v <- output_visible[[j]]
      if (is.factor(v)) v <- as.character(v)
      v <- trimws(v)
      match_bottom <- integer(0)
      if (n_rows >= 2) {
        # previous row values
        prev <- c(NA, v[-length(v)])
        # same indicates exact match with previous non-empty values
        same <- v == prev
        same[is.na(v) | is.na(prev)] <- FALSE
        same[v == "" | prev == ""] <- FALSE
        # rows where B matches A immediately above
        match_bottom <- which(same)
      }
      if (length(match_bottom)) {
        # rows forming pairs to highlight
        rows_pair <- sort(unique(c(match_bottom, match_bottom - 1L)))
        # optionally include row above blank if value is repeated in that pattern
        rows_above_blank <- (match_bottom - 2L)
        rows_above_blank <- rows_above_blank[rows_above_blank >= 1L &
                                               (is.na(v[rows_above_blank]) | v[rows_above_blank] == "")]
        # union of rows to style for this column
        rows_to_style <- sort(unique(c(rows_pair, rows_above_blank)))
        if (length(rows_to_style)) {
          # apply yellow highlight to selected cells (+1 for header row offset)
          openxlsx::addStyle(wb, "output", style = yellow,
                             rows = rows_to_style + 1L, cols = j,
                             gridExpand = FALSE, stack = TRUE)
        }
      }
    }
    # save workbook to specified path
    openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  }
  
  # return the visible clerical review data.frame
  output_visible
}

#' Join linked ids to A/B tables and reshape for clerical review / export
#'
#' @param nls list from `run_fastlink()` containing `$id_df`.
#' @param dfA,dfB data.frames.
#' @param source_label named chr c(data_source_a=..., data_source_b=...).
#' @param linking_var character of additional columns to carry through (present in A/B).
#' @param generate_xlsx logical. Write Excel?
#' @param output_path string.
#' @param arrange_desc logical. Sort by posterior descending?
#' @param sample_fraction numeric in (0,1].
#' @param rowid_a,rowid_b strings. Id columns in A/B.
#'
#' @return list(linked_data, reshaped_output)
link_join_and_reshape <- function(nls,
                                  dfA, dfB,
                                  source_label = c(data_source_a = NA, data_source_b = NA),
                                  linking_var,
                                  generate_xlsx = TRUE,
                                  output_path = "linked_output.xlsx",
                                  arrange_desc = FALSE,
                                  sample_fraction = 0.3,
                                  rowid_a = "rowid.a",
                                  rowid_b = "rowid.b") {
  
  # id_df holds {rowid_a, rowid_b, posterior} for matched pairs
  id_df <- nls$id_df
  
  # attach linking variables for dfA matched rows and ensure one row per id
  A_linked <- dfA |>
    dplyr::select(dplyr::all_of(c(rowid_a, "name_clean", "name_1", "name_2", linking_var))) |>
    dplyr::mutate(!!rlang::sym(rowid_a) := as.numeric(.data[[rowid_a]])) |>
    dplyr::rename_with(~ paste0(., ".a"), .cols = -dplyr::all_of(rowid_a)) |>
    dplyr::group_by(!!rlang::sym(rowid_a)) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$n)
  
  # attach linking variables for dfB matched rows and ensure one row per id
  B_linked <- dfB |>
    dplyr::select(dplyr::all_of(c(rowid_b, "name_clean", "name_1", "name_2", linking_var))) |>
    dplyr::mutate(!!rlang::sym(rowid_b) := as.numeric(.data[[rowid_b]])) |>
    dplyr::rename_with(~ paste0(., ".b"), .cols = -dplyr::all_of(rowid_b)) |>
    dplyr::group_by(!!rlang::sym(rowid_b)) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$n)
  
  # join id_df with enriched A/B data
  linked <- id_df |>
    dplyr::left_join(B_linked, by = rowid_b) |>
    dplyr::left_join(A_linked, by = rowid_a) |>
    # create explicit posterior.a / posterior.b columns for plotting/ordering
    dplyr::mutate(posterior.a = .data$posterior, posterior.b = .data$posterior) |>
    # rename rowid columns to consistent names (rowid.a/rowid.b)
    rename(rowid.a = rowid_a,
           rowid.b = rowid_b)
  
  # choose which posterior column to use for ordering
  arr_col <- if ("posterior.a" %in% names(linked)) "posterior.a"
  else if ("posterior.b" %in% names(linked)) "posterior.b" else NA_character_
  
  if (!is.na(arr_col)) {
    # sort by posterior ascending or descending depending on argument
    linked <- if (arrange_desc) dplyr::arrange(linked, dplyr::desc(.data[[arr_col]]))
    else dplyr::arrange(linked, .data[[arr_col]])
  } else {
    warning("No posterior column found for ordering.")
  }
  
  # reshape the linked data into A/B blocks for clerical review
  reshaped <- reshape_xy_to_end_example(
    df = linked,
    source_label = source_label,
    output_path = output_path,
    sample_fraction = sample_fraction,
    generate_xlsx = generate_xlsx
  )
  
  # return full linked table and the reshaped review table invisibly
  invisible(list(linked_data = linked, reshaped_output = reshaped))
}

# ------------------------------------------------------------
# Missingness pattern summary
# ------------------------------------------------------------

#' Summarise unique missingness combinations across selected columns
#'
#' @param df data.frame
#' @param vars character of columns to check (default: all)
#' @return tibble with one row per distinct NA pattern and count `n`.
analyse_missing_combinations <- function(df, vars = names(df)) {
  # keep only variables that exist in df
  vars <- intersect(vars, names(df))
  # create a logical matrix indicating which cells are NA
  miss_mat <- df[, vars, drop = FALSE] |>
    dplyr::mutate(dplyr::across(dplyr::everything(), is.na))
  miss_mat |>
    # group by each unique pattern of missingness across vars
    dplyr::group_by(dplyr::across(dplyr::everything())) |>
    # count how many rows share each pattern
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    # sort by frequency descending
    dplyr::arrange(dplyr::desc(.data$n))
}

# ------------------------------------------------------------
# Metrics across posterior thresholds (CR subset → full set)
# ------------------------------------------------------------

#' Summarise linkage quality across posterior thresholds
#'
#' Computes precision/recall on the clerically reviewed subset at a grid of
#' posterior probability thresholds, then extrapolates estimated counts of true
#' links, false discoveries, and false negatives to the full candidate set.
#' Also reports the linkage rate (predicted links / smaller source size).
#'
#' @param cr_data data.frame of candidate pairs with at least:
#'   - posterior: numeric in [0, 1] (match probability for the pair)
#'   - clerical_review: 1/0 or TRUE/FALSE (gold label on a reviewed subset)
#'   - data_source, rowid: only needed indirectly by `build_linked_unlinked()`
#' @param nls list-like object that contains `id_df`, a data.frame of ALL
#'   candidate pairs with columns `{rowid_a, rowid_b, posterior}`. Extra columns
#'   (e.g., gamma*) are ignored.
#' @param dfA data.frame for source A (must contain column named by `rowid_a`)
#' @param dfB data.frame for source B (must contain column named by `rowid_b`)
#' @param source_label named character vector of length 2 giving the values used
#'   in `cr_data$data_source` to identify which side each `rowid` refers to:
#'   `c(data_source_a = "<label for A>", data_source_b = "<label for B>")`
#' @param rowid_a character, the column name of the unique row id in `dfA`
#' @param rowid_b character, the column name of the unique row id in `dfB`
#' @param thresholds numeric vector of posterior cutoffs in [0, 1] at which to
#'   evaluate metrics (default: seq(0, 0.9, by = 0.1))
#'
#' @return data.frame with one row per threshold containing:
#'   - post_threshold: formatted threshold used (character)
#'   - linkage_rate: predicted links / min(nrow(dfA), nrow(dfB)) in percent (character)
#'   - total_predicted_links: integer count of pairs with posterior >= threshold
#'   - est_true_links: estimated true links among predicted (integer)
#'   - est_false_discoveries: estimated false positives among predicted (integer)
#'   - est_false_negatives: estimated false negatives (integer)
#'   - precision_hat: estimated precision on labelled subset, in percent (character)
#'   - recall_hat: estimated recall on labelled subset, in percent (character)
#'   - fdr_hat: estimated false discovery rate = 1 - precision, in percent (character)
#'   - fnr_hat: estimated false negative rate = 1 - recall, in percent (character)
summarise_cr_data <- function(cr_data, 
                              nls, 
                              dfA, 
                              dfB, 
                              source_label = c(data_source_a = NA, data_source_b = NA),
                              rowid_a,
                              rowid_b,
                              thresholds = seq(0, 0.9, by = 0.1)) {
  
  # cr_data must be a data.frame with posterior + clerical_review
  stopifnot(is.data.frame(cr_data))
  # cr_data must have: posterior, clerical_review (TRUE/FALSE/1/0)
  stopifnot(all(c("posterior", "clerical_review") %in% names(cr_data)))
  # thresholds sanity check
  stopifnot(is.numeric(thresholds), all(thresholds >= 0 & thresholds <= 1))
  
  # Keep only labelled rows and create binary truth indicator
  cr_lab <- cr_data |>
    # we only use rows with a clerical decision
    dplyr::filter(!is.na(clerical_review)) |>
    dplyr::transmute(
      # keep posterior field
      posterior,
      # convert clerical_review into TRUE/FALSE for matched vs non-matched
      truth_linked = clerical_review == 1
    )
  
  # Per-threshold metrics on labelled data -> extrapolate to full set
  out_list <- lapply(thresholds, function(t) {
    
    # classify labelled records as predicted link / non-link at this threshold
    df_t <- cr_lab |>
      dplyr::mutate(predicted_link = posterior >= t)
    
    # true positives, false positives, and false negatives in labelled subset
    tp <- sum(df_t$predicted_link & df_t$truth_linked, na.rm = TRUE)
    fp <- sum(df_t$predicted_link & !df_t$truth_linked, na.rm = TRUE)
    fn <- sum(!df_t$predicted_link & df_t$truth_linked, na.rm = TRUE)
    
    # estimated precision and recall on the clerically labelled subset
    precision_hat <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall_hat    <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    
    # Apply threshold to ALL candidate pairs (with clerical overrides)
    linked_unlinked <- build_linked_unlinked(cr_data,
                                             dfA, dfB,
                                             nls,
                                             post_threshold = t,
                                             source_label = source_label,
                                             rowid_a = rowid_a,
                                             rowid_b = rowid_b)
    
    # number of links predicted over full candidate set
    total_predicted <- linked_unlinked$linked_data |> nrow()
    # size of smaller source table for linkage-rate denominator
    smallest_dataset <- min(dfA |> nrow(), dfB |> nrow())
    
    # Linkage rate (% of smaller dataset that links at this threshold)
    linkage_rate <- total_predicted / smallest_dataset * 100
    
    # Extrapolate totals on the full set using precision/recall from labelled subset
    est_true_links        <- if (!is.na(precision_hat)) precision_hat * total_predicted else NA_real_
    est_false_discoveries <- if (!is.na(precision_hat)) total_predicted - est_true_links else NA_real_
    est_false_negatives   <- if (!is.na(recall_hat) && !is.na(est_true_links)) (est_true_links / recall_hat) - est_true_links else NA_real_
    
    # format and assemble summary row
    data.frame(
      post_threshold         = sprintf("%.2f", t),
      linkage_rate           = sprintf("%.1f", linkage_rate),
      total_predicted_links  = signif(as.integer(round(total_predicted)), 3),
      est_true_links         = as.integer(round(est_true_links)),
      est_false_discoveries  = as.integer(round(est_false_discoveries)),
      est_false_negatives    = as.integer(round(est_false_negatives)),
      precision_hat          = sprintf("%.1f", precision_hat * 100),
      recall_hat             = sprintf("%.1f", recall_hat * 100),
      fdr_hat                = if (!is.na(precision_hat)) sprintf("%.1f", (1 - precision_hat) * 100) else NA_real_,
      fnr_hat                = if (!is.na(recall_hat)) sprintf("%.1f", (1 - recall_hat) * 100) else NA_real_
    )
  })
  
  # stack threshold-specific summaries into one data.frame
  dplyr::bind_rows(out_list)
}



#' Build linked and unlinked datasets at a given posterior threshold
#'
#' Applies the posterior threshold to the candidate pair table (`nls$id_df`) to
#' produce a table of predicted links and two "unlinked" datasets for source A
#' and source B. Clerical review decisions in `cr_data` override the threshold:
#'   - rows marked as accept (clerical_review == 1) are forced linked,
#'   - rows marked as reject (clerical_review == 0) are forced unlinked.
#'
#' @param cr_data data.frame of reviewed pairs with at least:
#'   - data_source: labels identifying whether each `rowid` belongs to A or B,
#'     matching `source_label` values
#'   - rowid: the id value within the labelled source
#'   - clerical_review: 1/0 indicating accept/reject; NAs are left as-is
#' @param dfA data.frame for source A (must contain column named by `rowid_a`)
#' @param dfB data.frame for source B (must contain column named by `rowid_b`)
#' @param nls list-like object that contains `id_df`, a data.frame of ALL
#'   candidate pairs with columns `{rowid_a, rowid_b, posterior}`. Additional
#'   columns (e.g., gamma*) are allowed and will be dropped from unlinked sets.
#' @param post_threshold numeric in [0, 1], posterior cutoff for linking
#' @param source_label named character vector length 2 giving the values used in
#'   `cr_data$data_source` to identify which side the `rowid` refers to:
#'   `c(data_source_a = "<label for A>", data_source_b = "<label for B>")`
#' @param rowid_a character, the column name of the unique row id in `dfA`
#' @param rowid_b character, the column name of the unique row id in `dfB`
#'
#' @return list with three elements:
#'   - linked_data: data.frame of `{rowid_a, rowid_b}` pairs predicted as links
#'   - dfA_unlinked_data: data.frame of A records predicted unlinked (and A records
#'       that never appeared in `id_df`, via anti_join), with gamma/posterior removed
#'   - dfB_unlinked_data: same for B
build_linked_unlinked <- function(cr_data,
                                  dfA, dfB,
                                  nls,
                                  post_threshold,
                                  source_label = c(data_source_a = NA, data_source_b = NA),
                                  rowid_a,
                                  rowid_b){
  
  # Fill forward a "1" decision by one row within cr_data (simple lag approach)
  # if the row below is NA but the one above is 1, propagate the 1 down
  cr_data <- cr_data %>%
    dplyr::mutate(clerical_review_2 = dplyr::lag(ifelse(clerical_review == 1, 1, clerical_review)),
                  clerical_review   = ifelse(is.na(clerical_review), clerical_review_2, clerical_review)) %>%
    dplyr::select(-clerical_review_2) 
  
  # Candidate pair table with posterior probabilities
  id_df <- nls$id_df %>%
    dplyr::ungroup()
  
  # Row id sets with clerical overrides for A
  # all rowids from source A that have been accepted
  dfA_ids_accept <- cr_data %>%
    dplyr::filter(data_source == source_label["data_source_a"],
                  clerical_review == 1) %>%
    dplyr::select(rowid) %>% dplyr::pull()
  
  # all rowids from source A that have been rejected
  dfA_ids_reject <- cr_data %>%
    dplyr::filter(data_source == source_label["data_source_a"],
                  clerical_review == 0) %>%
    dplyr::select(rowid) %>% dplyr::pull()
  
  # Row id sets with clerical overrides for B
  # all rowids from source B that have been accepted
  dfB_ids_accept <- cr_data %>%
    dplyr::filter(data_source == source_label["data_source_b"],
                  clerical_review == 1) %>%
    dplyr::select(rowid) %>% dplyr::pull()
  
  # all rowids from source B that have been rejected
  dfB_ids_reject <- cr_data %>%
    dplyr::filter(data_source == source_label["data_source_b"],
                  clerical_review == 0) %>%
    dplyr::select(rowid) %>% dplyr::pull()
  
  # Predicted links: threshold OR clerical accept, minus clerical rejects
  linked_data <- dfA %>%
    # rename dfA id column to rowid.a for joining with id_df
    dplyr::rename(rowid.a = {{rowid_a}}) %>%
    # keep only records which appear in candidate pairs
    dplyr::filter(.data$rowid.a %in% c(id_df[[rowid_a]])) %>%
    # join in B ids and posterior from id_df
    dplyr::left_join(id_df %>% 
                       dplyr::select({{rowid_a}}, {{rowid_b}}, posterior) %>%
                       dplyr::rename(rowid.a = {{rowid_a}},
                                     rowid.b = {{rowid_b}}),
                     by = "rowid.a") %>%
    # keep rows where posterior ≥ threshold or in accept list, and not in reject list
    dplyr::filter(posterior >= post_threshold | .data$rowid.a %in% dfA_ids_accept,
                  !(.data$rowid.a %in% dfA_ids_reject)) %>%
    # rename rowid.a/rowid.b back to original column names
    dplyr::rename(!!rowid_a := .data$rowid.a,
                  !!rowid_b := .data$rowid.b) %>%
    # keep only final id pair columns
    dplyr::select({{rowid_a}}, {{rowid_b}})
  
  # Records that never appeared in candidate pairs are automatically unlinked
  # A-side records that are not present in id_df are unlinked by default
  dfA_anti_join <- dfA %>%
    dplyr::anti_join(id_df, by = rowid_a)
  
  # B-side records that never appeared in id_df
  dfB_anti_join <- dfB %>%
    dplyr::anti_join(id_df, by = rowid_b)
  
  # for convenience, build symbols for rowid columns
  col_a <- rlang::sym(rowid_a)
  col_b <- rlang::sym(rowid_b)
  
  # Unlinked sets at the threshold, plus the anti-join records
  dfA_unlinked_data <- dfA %>%
    # restrict to A records that appear in candidate pairs
    dplyr::filter(!!col_a %in% c(id_df[[rowid_a]])) %>%
    # attach posterior probabilities vector from id_df
    dplyr::mutate(posterior = id_df$posterior) %>%
    # keep rows below threshold or explicitly rejected, excluding accepted ones
    dplyr::filter(posterior < post_threshold | !!col_a %in% dfA_ids_reject,
                  !(!!col_a %in% dfA_ids_accept)) %>%
    # drop fastLink gamma/posterior columns from unlinked output
    dplyr::select(-c(dplyr::contains("gamma"), "posterior")) %>%
    # append anti-join (never-matched) records
    rbind(dfA_anti_join)
  
  dfB_unlinked_data <- dfB %>%
    # restrict to B records that appear in candidate pairs
    dplyr::filter(!!col_b %in% c(id_df[[rowid_b]])) %>%
    # attach posterior vector
    dplyr::mutate(posterior = id_df$posterior) %>%
    # apply threshold and clerical overrides on B side
    dplyr::filter(posterior < post_threshold | !!col_b %in% dfB_ids_reject,
                  !(!!col_b %in% dfB_ids_accept)) %>%
    # drop fastLink gamma/posterior fields
    dplyr::select(-c(dplyr::contains("gamma"), "posterior")) %>%
    # append never-matched B records
    rbind(dfB_anti_join)
  
  # return linked pairs and remaining unlinked sets
  list(
    linked_data       = linked_data,
    dfA_unlinked_data = dfA_unlinked_data,
    dfB_unlinked_data = dfB_unlinked_data
  )
}


# ------------------------------------------------------------
# Plot helper (uses summarise_cr_thresholds)
# ------------------------------------------------------------

#' Build a dual-axis plot of linkage metrics vs threshold
#'
#' @param cr_batch data.frame with clerical labels and posterior.
#' @param post_thresholds numeric vector.
#' @param left_axis_limits numeric length-2 (0-100).
#' @param coeff numeric or NULL (right-axis scaling). If NULL, auto-compute.
#' @param metric_colors named vector for left-axis series.
#' @param dfA,dfB,linked_data,sample_fraction passed through to `summarise_cr_thresholds`.
#'
#' @return list(plot = ggplot, data = results_tbl, coeff = used_scale)
plot_linkage_vs_threshold <- function(
    cr_batch,
    post_thresholds   = seq(0, 0.99, by = 0.01),
    left_axis_limits  = c(0, 100),
    coeff             = NULL,
    metric_colors     = c(
      "Linkage rate (%)" = "#1b9e77",
      "FDR (%)"          = "#d95f02",
      "FNR (%)"          = "#7570b3"
    ),
    dfA, dfB, linked_data, sample_fraction
) {
  # verify expected columns for clerical review data
  stopifnot(all(c("clerical_review","posterior") %in% names(cr_batch)))
  
  # compute linkage metrics across thresholds using summarise_cr_thresholds()
  results_tbl <- summarise_cr_thresholds(
    cr_data = cr_batch,
    thresholds = post_thresholds,
    dfA = dfA,
    dfB = dfB,
    linked_data = linked_data,
    sample_fraction = sample_fraction
  )
  
  # prepare long-form data for plotting left-axis metrics
  metrics_long <- results_tbl |>
    dplyr::transmute(
      post_threshold,
      `Linkage rate (%)` = as.numeric(full_linkage_rate_percent_est),
      `FDR (%)`          = as.numeric(sample_fdr_percent),
      `FNR (%)`          = as.numeric(sample_fnr_percent),
      total_linked       = as.numeric(full_true_links_est)  # proxy for right axis
    ) |>
    tidyr::pivot_longer(cols = c(`Linkage rate (%)`,`FDR (%)`,`FNR (%)`),
                        names_to = "metric", values_to = "value")
  
  # compute coeff if needed: map right-axis (total_linked) into left scale
  max_left  <- max(left_axis_limits, na.rm = TRUE)
  max_right <- max(results_tbl$full_true_links_est, na.rm = TRUE)
  if (is.null(coeff) || !is.finite(coeff)) {
    # scale factor to align maximum of right-axis series with left-axis range
    coeff <- if (is.finite(max_right) && max_right > 0) max_left / max_right else 1
  }
  
  # build dual-axis plot: rate metrics on left, total links on right
  gg <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = post_threshold)) +
    ggplot2::geom_line(ggplot2::aes(y = value, colour = metric), linewidth = 1) +
    ggplot2::geom_line(data = results_tbl,
                       ggplot2::aes(y = as.numeric(full_true_links_est) * coeff),
                       linewidth = 1, colour = "#000000") +
    ggplot2::scale_y_continuous(name = "Rate (%)",
                                limits = left_axis_limits,
                                sec.axis = ggplot2::sec_axis(~ . / coeff, name = "Total linked (est)")) +
    ggplot2::scale_x_continuous(name = "Posterior threshold",
                                limits = c(min(results_tbl$post_threshold), max(results_tbl$post_threshold))) +
    ggplot2::scale_color_manual(values = metric_colors, name = "Metric") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "top",
                   axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 10)))
  
  # return ggplot, underlying metric table, and scaling coefficient
  list(plot = gg, data = results_tbl, coeff = coeff)
}
