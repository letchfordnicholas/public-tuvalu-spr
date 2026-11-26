# Tuvalu Statistical Population Register: data cleaning and record linkage

This repository contains the R code developed for the Tuvalu Statistical Population Register (SPR) experiment. The project explored whether census and administrative records could be combined to produce an up-to-date picture of Tuvalu's population, and what improvements would be needed for a reliable register.

The work was undertaken with Tuvalu's Central Statistics Division (CSD) and the Pacific Community (SPC), through the Statistical Innovation and Capacity Building in the Pacific Islands (PacStat) project. The code was developed by Nicholas Letchford.

The repository is intended to document the methods used in the experiment and provide a starting point for others working on population registers and record linkage. **It is research and prototype code, not an operational population register.** The scripts need review and adaptation before they are run on another dataset or used to produce official statistics. In particular, the supplied scripts contain issues described under [Known implementation issues](#known-implementation-issues).

## Project overview

The SPR experiment brought together five types of data:

- The 2022 Population and Housing Census, used as the initial population base.
- Birth and death records.
- International arrival and departure records.

The data were cleaned and deduplicated, and records referring to the same person were linked using probabilistic matching. Potential matches were checked through clerical review. A separate exercise linked the 2022 and 2025 censuses to examine how linkage performed when more identifying information was available.

The [final report](https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/01/01263827144a691bc20cc5088a00a524.pdf) describes the full project, findings and recommendations. In that report, linkage rates were 51.7% for arrivals, 67.3% for departures and 50.5% for deaths; the 2022–2025 census linkage achieved 73.5%. The census comparison was restricted to people in the 2025 census born before 2023. These are historical results from the experiment, not a measure of the performance of a new application of the code. The report concluded that data quality and linkage limitations meant the prototype was not yet suitable to replace a census.

## What the code does

The scripts provide a workflow to:

1. Read census and administrative data and standardise fields used for linkage, including names, dates of birth and sex.
2. Identify potential duplicate records within each source using the `fastLink` R package.
3. Prepare duplicate records for manual review and add selected records back into the data used for linkage.
4. Link records across sources using probabilistic matching, with different combinations of matching variables across linkage batches.
5. Create spreadsheets for clerical review, summarise reviewed matches and use review decisions alongside probability thresholds.
6. Assemble linked and unlinked records, create a prototype SPR dataset and prepare tables for a SQL Server database.

The census-to-census script also examines unlinked records and uses successive batches with different fields, such as names, date of birth, location, relationship to household head and GPS coordinates. The scripts are examples of an iterative process; they are not a fully automated pipeline.

## Repository contents

The scripts expect to be stored in a folder called `code` under the project root. The descriptions below refer to the supplied files.

| File | Purpose |
| --- | --- |
| [`code cleaning functions.R`](code/code%20cleaning%20functions.R) | Shared functions for cleaning, deduplication, linkage, clerical review and summaries. |
| [`2022 census data cleaning and deduplication.R`](code/2022%20census%20data%20cleaning%20and%20deduplication.R) | Prepares and deduplicates the 2022 census person and household data. |
| [`2025 census data cleaning and deduplication.R`](code/2025%20census%20data%20cleaning%20and%20deduplication.R) | Prepares and deduplicates the 2025 census person, household and dwelling data. |
| [`data cleaning and deduplication.R`](code/data%20cleaning%20and%20deduplication.R) | Prepares and deduplicates birth, death, arrival and departure records. |
| [`spr data linkage.R`](code/spr%20data%20linkage.R) | Illustrates construction of the prototype SPR from the 2022 census and administrative data. |
| [`fastlink census data linkage.R`](code/fastlink%20census%20data%20linkage.R) | Links the 2022 and 2025 censuses over five iterative batches. |
| [`create spr database.R`](code/create%20spr%20database.R) | Prepares SPR and source tables and writes them to SQL Server. |

The accompanying *Sequencing and code executions instructions* document provides a fuller walkthrough of the original process. Where the manual and code differ, inspect the code and the issues below before running it.

## Data and confidentiality

The original census and administrative records, clerical review files and linked datasets are confidential and are not included. Access to the code does not provide access to the underlying data, and the published results cannot be reproduced from this repository alone.

Raw records, cleaned files, review spreadsheets, linked outputs and database backups can all contain identifying information. Keep them in an approved secure environment and **do not commit them to a public repository**. The directory structure below shows where the scripts expect local files; these data and output folders should be excluded from version control. Any synthetic data used for testing should be clearly labelled as such.

## Requirements

- R 4.1 or later is needed for the native pipe (`|>`). RStudio is useful for running the scripts interactively.
- The code uses the packages below. Package versions were not recorded in the supplied files, so compatibility with current versions has not been established.
- The optional database step additionally requires SQL Server, a suitable ODBC driver, database permissions and a connection configured for the local environment.

For a new R environment, the packages referenced by the scripts can be installed with:

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "readxl", "readr", "haven", "janitor",
  "rlang", "lubridate", "here", "fastLink", "purrr", "tibble", "tidyselect",
  "openxlsx", "ggplot2", "cowplot", "DBI", "odbc"
))
```

The scripts use `here::here()` to find files relative to the project root. Open an RStudio project at the repository root, or otherwise ensure that `here::here()` resolves to that directory. Run the scripts in an R session with the root as the working directory; one of the cleaning scripts also uses a relative path directly.

## Preparing the working directory

Place the R scripts in `code/` and configure a secure local working directory similar to the following. Folder and file names in the scripts may need to be changed to match the available data.

```text
project root/
├── README.md
├── code/                         # R scripts
├── data/                         # local, confidential; do not publish
│   ├── raw data/
│   │   ├── 2022 Census/
│   │   ├── 2025 census/
│   │   ├── births and deaths/
│   │   └── immigration/
│   ├── cleaned data/
│   │   ├── 2022 Census/
│   │   ├── 2025 census/
│   │   ├── births and deaths/
│   │   └── immigration/
│   └── linked data/
│       ├── spr linkage/
│       └── census data linkage/
└── figures/                      # local outputs; review before publishing
```

The source-specific cleaning scripts expect particular Excel and Stata files and column names. Update the paths, field names, codes, date formats and filtering rules for the data being used. The same information must have consistent field names and values across sources before linkage. Stable source-specific identifiers are needed to trace linked records back to their source.

**Check capitalisation before running:** the supplied scripts refer to `code cleaning functions.r`, while the supplied file ends in `.R`. They also use both `2025 census` and `2025 Census`, and different capitalisation in some census output filenames. These distinctions matter on case-sensitive systems; make the references and actual names consistent.

## Workflow

Run the scripts in sections and inspect the intermediate outputs. Do not run every file as a single unattended job: duplicate review and record-linkage review require human decisions. Several scripts start with `rm(list = ls())`, which clears objects from the current R workspace. Start in a suitable clean session, then retain the same session while working through each linkage analysis and its review batches.

### 1. Clean and deduplicate each source

Run the relevant cleaning script for the source being prepared. In each script, source-specific processing is followed by `data_clean_fn()` from the shared functions file. The function standardises fields, creates name and date components, and uses `fastLink` to identify potential duplicates within the source.

For each source, the function writes files with prefixes:

- `cleaned_all` — all cleaned records, with duplicate-related fields.
- `dedup` — records not flagged as belonging to a duplicate group.
- `duplicates` — records in potential duplicate groups, for review.

**Review the duplicate groups before linkage.** In the supplied implementation, the `dedup` output excludes records flagged as members of a duplicate group; it does not automatically retain one representative from each group. Review the `duplicates` CSV and enter `1` in `clerical_review` for each record that should be retained. For example, retain one record for a group representing the same person, or retain multiple records if the review establishes that they are different people. Save a reviewed copy with the `_labelled.csv` suffix expected by `load_and_process_data()`.

For example, for a source label of `arrival data`, the loader expects both `dedup arrival data.csv` and `duplicates arrival data_labelled.csv` in the same source folder. It adds rows marked `1` in the reviewed file to the `dedup` records. It expects the labelled file even if no rows are selected. Confirm filenames and column structure before proceeding.

### 2. Link records and conduct clerical review

The SPR and census-comparison scripts use the same general sequence:

1. Load the cleaned data and reviewed duplicates with `load_and_process_data()`.
2. Define the linking fields, string fields, source IDs and review sample fraction for the batch. Matching fields should have the same names and compatible values in both sources.
3. Run `run_fastlink()` to generate candidate links and posterior match probabilities. The wrapper removes candidates with ambiguous repeated row indices to produce a one-to-one mapping.
4. Run `link_join_and_reshape()` to create a review spreadsheet. The review sample is drawn in proportions across bands of posterior probabilities. For a new review file, set `generate_cr_file <- TRUE` before the relevant call. The supplied linkage scripts initially set this to `FALSE` and expect review files to exist already.
5. In the workbook, enter `1` in the `clerical_review` cell on the first row of a candidate pair if the records represent the same person, or `0` if they do not. Preserve the layout, save the file as `.xlsx` and close it before importing it.
6. Read the completed workbook, inspect `summarise_cr_data()` where used, choose an appropriate threshold, and run `build_linked_unlinked()` to create linked and remaining unlinked datasets. Reviewed decisions are intended to override the threshold for those pairs.
7. For further batches, apply revised linking variables to the remaining unlinked records. Combine the accepted linked IDs only after checking the results of each batch.

**Do not regenerate a completed review workbook at the same path:** the export uses `overwrite = TRUE`. Keep the in-memory linkage objects, source data and reviewed workbook together for the duration of each analysis. The review sampling function uses a fixed seed by default, but reproducibility also depends on the same input records, their order, code, settings and package versions.

A higher posterior threshold generally accepts fewer candidate links; a lower threshold accepts more but can introduce more false matches. Choose thresholds using the review results and the purpose of the analysis, rather than assuming that a threshold from this experiment will be suitable elsewhere. Review-based quality estimates concern the candidate pairs considered and do not measure all true links that the matching process failed to propose.

### 3. Assemble the prototype SPR

In `spr data linkage.R`, the broad sequence is to link the 2022 census to arrivals, add birth records, then link departures and deaths. The script writes a prototype `spr.csv` containing source IDs and other fields under `data/linked data/spr linkage/`. It also creates objects for exploring population changes over time.

In this prototype, unlinked arrivals were treated as additions to the population, and births were added directly; unlinked departures and deaths could not be assigned to a person in the register. These assumptions and incomplete linkage can affect population estimates. The final report discusses the resulting limitations in detail. Review the identifier-generation and linkage issues below before using the assembled dataset.

### 4. Link the 2022 and 2025 censuses

`fastlink census data linkage.R` links the two cleaned censuses in five batches. It starts with name, date of birth, sex and location fields, then tests alternative combinations on records left unlinked, including removing name suffixes and using household relationship, birthplace, GPS coordinates or enumeration area. The script restricts the 2025 data to people born in or before 2022 for the comparison.

Each batch needs its own completed review file. The script writes final unlinked datasets and combines linked IDs in memory as `combined_linked_data`; save that object separately in the secure working environment if needed.

### 5. Optional: prepare SQL Server tables

`create spr database.R` reads the prototype SPR and cleaned source files, connects to SQL Server through ODBC, creates a database if needed, and writes separate tables. Adapt the connection settings, authentication, encryption and filenames to the approved environment. The script currently uses local SQL Server settings, trusted authentication and `Encrypt = "no"`; these settings should not be assumed appropriate for another system.

The `dbWriteTable()` calls use `overwrite = TRUE` and can replace existing tables. Use a suitable test database and verify the target before running them. The script writes tables; it does not itself create a SQL Server `.bak` backup.

## Known implementation issues

The supplied files are a snapshot of project code and require technical review before execution. In addition to the filename differences noted above:

- The fourth census-linkage batch passes a `thresholds` argument to `build_linked_unlinked()`, but that function does not accept it. Remove or relocate the argument as appropriate before running that batch.
- In `build_linked_unlinked()`, posterior values are attached to the unlinked source data by position. Source rows and candidate pairs may be ordered differently, which can assign a probability to the wrong record. Review and correct this logic, including the handling of clerical overrides, and validate the linked and unlinked sets before using the results.
- The SPR script generates IDs for unlinked arrivals and birth records using the same census-ID offset. If IDs overlap between those sources, generated person IDs can collide. Use a collision-free ID strategy and check uniqueness before assembling a register.
- The database script refers to `death_data_` when writing the death table, although the object created earlier is `death_data`. Correct the reference and test the complete database step before use.
- Settings in the supplied scripts are not identical to every setting described in the final report. For example, the arrival-linkage threshold in the supplied SPR script is `0`, while the report describes `0.8` for the reported arrival analysis. Confirm the intended parameters and review decisions for the analysis being undertaken; do not assume the scripts will reproduce the report's figures unchanged.

These points are not a complete code audit. Test any adaptation with appropriate non-sensitive data and independently check deduplication, candidate pairs, review decisions, unique IDs and final counts before processing real records.

## Outputs and interpretation

Depending on which scripts are run, the workflow can produce cleaned and deduplicated CSVs, duplicate-review CSVs, clerical-review workbooks, linked ID pairs, unlinked records, a prototype SPR CSV and SQL Server tables. These files may contain personal information and must be handled accordingly.

The final report identifies inconsistent names and dates of birth, limited address information and the absence of a common personal identifier as important barriers to linkage. It recommends improving the consistency of identifying data, developing an address reference, strengthening birth registration and building local technical capacity. An unmatched record should not automatically be interpreted as a new resident without considering the quality and coverage of the source data.

## Report and acknowledgement

Letchford, N. (2025). *Data Integration for the Tuvalu Statistical Population Register: Final Report*. Pacific Community, Nouméa, New Caledonia. [Read the report](https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/01/01263827144a691bc20cc5088a00a524.pdf).

The experiment was a collaboration between Tuvalu's Central Statistics Division and SPC, with co-funding from the Government of Tuvalu and SPC through the World Bank-funded PacStat project. The report acknowledges the participating agencies, staff and the people of Tuvalu.

## Licence

No software licence was specified in the supplied materials. Confirm the applicable code ownership and licence before reusing or redistributing the code. The final report is subject to the terms stated in that publication.
