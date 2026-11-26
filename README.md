# Tuvalu Statistical Population Register: data cleaning and record linkage

This repository contains selected R scripts developed for the Tuvalu Statistical Population Register (SPR) feasibility study. The project investigated whether census and administrative records could be integrated to establish and maintain an up-to-date picture of Tuvalu's population, and identified improvements needed to support a reliable statistical population register.

The work was undertaken in collaboration with Tuvalu's Central Statistics Division (CSD) and the Pacific Community (SPC), through the Statistical Innovation and Capacity Building in the Pacific Islands (PacStat) project. The code was developed by Nicholas Letchford.

## Purpose and scope of this repository

The scripts have been copied from the original private project repository to provide a publicly accessible example of the statistical methods and programming approaches developed during the assignment. They demonstrate data cleaning and harmonisation, deduplication, probabilistic record linkage, clerical review, data integration and database preparation.

The original assignment was an exploratory feasibility study rather than the development of a production-ready statistical population register. The scripts reflect an iterative research process in which alternative matching variables, linkage methods and parameter settings were investigated in response to the characteristics of the available data. Human review was an integral part of the methodology, particularly where the available identifying information was incomplete or inconsistent.

The confidential source data, intermediate datasets, clerical review files and final outputs are not included in this public repository. The repository is therefore intended as a reference for the methods and technical approaches used, rather than a standalone reproducible version of the original analysis.

Further review, testing and adaptation would be required before applying the scripts to other datasets or incorporating them into an operational statistical production system. See [Technical considerations for reuse](#technical-considerations-for-reuse).

## Project overview

The SPR experiment integrated five types of data:

- The 2022 Population and Housing Census, which provided the initial population base.
- Birth records.
- Death records.
- International arrival records.
- International departure records.

The records were cleaned and deduplicated before probabilistic linkage, with clerical review used to assess candidate matches. A separate comparison of the 2022 and 2025 censuses investigated linkage performance when additional identifying information was available.

The [final report](https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/01/01263827144a691bc20cc5088a00a524.pdf) presents the methodology, findings and recommendations. The reported linkage rates were 51.7% for arrivals, 67.3% for departures and 50.5% for deaths. The census-to-census comparison achieved 73.5%, restricting the 2025 census population to individuals born before 2023.

These figures describe the original experiment, not the expected performance of the scripts on another dataset. The study concluded that limitations in source data quality and linkage completeness meant the prototype was not yet suitable as a replacement for a census.

## What the code does

The scripts illustrate the following workflow:

1. Import census and administrative datasets and harmonise identifying variables, including names, dates of birth and sex.
2. Use the `fastLink` R package to identify potential duplicates within individual data sources.
3. Generate duplicate-review files and reintegrate records selected through manual review.
4. Perform probabilistic linkage between datasets, testing different combinations of matching variables across successive batches.
5. Generate Excel workbooks for clerical review, summarise review results and incorporate review decisions alongside probability-based matching thresholds.
6. Combine linked and unlinked records to construct a prototype SPR dataset and prepare tables for a SQL Server database.

The census-to-census analysis also examines unmatched records and tests successive combinations of identifying information, including names, dates of birth, location, relationship to the household head and GPS coordinates. These scripts illustrate an iterative statistical investigation rather than a fully automated production pipeline.

## Repository contents

The R scripts are located in `code/` relative to the project root.

| File | Purpose |
| --- | --- |
| [`code cleaning functions.R`](code/code%20cleaning%20functions.R) | Reusable functions for cleaning, deduplication, linkage, clerical review and summaries. |
| [`2022 census data cleaning and deduplication.R`](code/2022%20census%20data%20cleaning%20and%20deduplication.R) | Cleaning and deduplication of 2022 census person and household data. |
| [`2025 census data cleaning and deduplication.R`](code/2025%20census%20data%20cleaning%20and%20deduplication.R) | Cleaning and deduplication of 2025 census person, household and dwelling data. |
| [`data cleaning and deduplication.R`](code/data%20cleaning%20and%20deduplication.R) | Preparation of birth, death, arrival and departure records. |
| [`spr data linkage.R`](code/spr%20data%20linkage.R) | Construction of the prototype SPR using the 2022 census and administrative records. |
| [`fastlink census data linkage.R`](code/fastlink%20census%20data%20linkage.R) | Linkage of the 2022 and 2025 censuses through five iterative batches. |
| [`create spr database.R`](code/create%20spr%20database.R) | Preparation and transfer of SPR and source tables to SQL Server. |

A separate *Sequencing and code executions instructions* document from the original project provides a more detailed walkthrough of the workflow. It is not included in the public repository. If working with the original documentation, check that the instructions correspond to the script versions being used.

## Data and confidentiality

The census and administrative data used in the study contain confidential personal information. The public repository contains code only, not the original datasets, intermediate data, completed review files or final outputs. The published findings cannot be reproduced from the public repository alone.

All raw data, processed datasets, review spreadsheets, linkage outputs and database backups should be stored and processed within an approved secure environment. They must not be committed to a public repository. Exclude local data and output directories from version control, and clearly identify any synthetic datasets used for testing.

## Requirements

- R 4.1 or later is required for the native pipe operator (`|>`).
- RStudio is useful for interactive execution of the scripts.
- The package versions used in the original environment are not recorded in the public scripts; compatibility with newer versions should be checked before reuse.
- The optional database stage requires SQL Server, an appropriate ODBC driver, authorised database access and locally configured connection settings.

The R packages referenced by the scripts can be installed using:

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "readxl", "readr", "haven", "janitor",
  "rlang", "lubridate", "here", "fastLink", "purrr", "tibble", "tidyselect",
  "openxlsx", "ggplot2", "cowplot", "DBI", "odbc"
))
```

The scripts primarily use `here::here()` for paths relative to the project root. Open the RStudio project at the repository root, or configure the session so that `here::here()` resolves to the correct location. Set the working directory to the root as well, since one cleaning script uses a relative path directly.

## Preparing the working directory

The following structure illustrates the local project directories expected by the scripts. The data and figures directories are not supplied with this repository.

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

The source-specific cleaning scripts are configured for the Excel and Stata files, field names and coding conventions encountered during the original study. Adapt file paths, column names, date formats, classifications and filtering rules to the available data. Linkage variables should have consistent names and compatible representations across sources, and stable source-specific identifiers should be retained to allow records to be traced to their origin.

On case-sensitive systems, check the consistency of file and folder names before execution. The scripts refer to `code cleaning functions.r`, whereas the file has the extension `.R`. They also contain variations in the capitalisation of `2025 census` and some census output filenames.

## Workflow

The workflow is designed for interactive execution, with intermediate results reviewed before progressing to subsequent stages. Duplicate resolution and linkage assessment involve human decisions and should not be treated as a single unattended process.

Several scripts begin with `rm(list = ls())`, which clears the R workspace. Start in an appropriate clean session and retain the relevant objects throughout each linkage analysis and its associated review batches.

### 1. Clean and deduplicate each source

Execute the cleaning script corresponding to the source dataset. Following source-specific preparation, the shared function `data_clean_fn()` standardises variables, derives name and date components and uses `fastLink` to identify possible duplicate records.

Three types of output are produced, identified by their filename prefixes:

| Prefix | Contents |
| --- | --- |
| `cleaned_all` | All cleaned records, including duplicate-related fields. |
| `dedup` | Records not identified as belonging to a potential duplicate group. |
| `duplicates` | Records in potential duplicate groups, for manual review. |

The `dedup` file excludes all records flagged as belonging to a potential duplicate group; it does not automatically select one representative record from each group. Review the `duplicates` CSV and enter `1` in the `clerical_review` field for records that should be retained. This may mean selecting one representative of a confirmed duplicate group, or retaining multiple records where the group includes distinct individuals.

Save the reviewed file using the `_labelled.csv` suffix expected by `load_and_process_data()`.

For example, for a source labelled `arrival data`, the loader expects the following files in the corresponding source folder:

- `dedup arrival data.csv`
- `duplicates arrival data_labelled.csv`

The loader combines the deduplicated records with reviewed records marked `1`. It expects the labelled file even when no records have been selected for reintegration. Confirm filenames and column structures before loading.

### 2. Link records and conduct clerical review

The SPR analysis and census-to-census comparison follow a common iterative process:

1. Import the cleaned datasets and reviewed duplicate records using `load_and_process_data()`.
2. Specify the linkage variables, string-matching fields, source identifiers and review sample fraction for the batch. Ensure that corresponding linkage fields have compatible names and values.
3. Generate candidate matches and posterior match probabilities using `run_fastlink()`. The wrapper removes candidates with ambiguous repeated row indices to construct a one-to-one mapping.
4. Use `link_join_and_reshape()` to prepare an Excel workbook for clerical review. Candidate pairs are sampled proportionally across posterior-probability bands. To create a new review file, set `generate_cr_file <- TRUE` before the relevant call. The supplied linkage scripts initially set it to `FALSE`, assuming completed review files already exist.
5. Review the candidate pairs and enter `1` in the `clerical_review` cell on the first row of a pair if both records refer to the same person, or `0` otherwise. Preserve the workbook layout, save it as `.xlsx` and close it before importing the completed review.
6. Import the reviewed workbook, examine the summaries from `summarise_cr_data()` where used, select an appropriate probability threshold and run `build_linked_unlinked()` to separate accepted links from remaining records. The intended logic is for explicit clerical review decisions to take precedence over probability thresholds for reviewed pairs; verify this behaviour before reuse.
7. Where appropriate, test alternative matching variables on the remaining unlinked records. Inspect each batch before combining the accepted linked identifiers.

**Preserve completed review workbooks.** The export uses `overwrite = TRUE`, so regenerating a workbook at the same path can replace existing review decisions. Retain the corresponding source data, linkage objects and reviewed workbook together. Although the review-sampling function uses a fixed seed by default, reproducibility also depends on the inputs, row ordering, code, parameter settings and package versions.

Selection of a matching threshold involves balancing linkage coverage against the risk of false matches. Lower thresholds generally accept more candidate pairs but may introduce incorrect links, while higher thresholds may exclude genuine matches. Thresholds should be informed by the review results and the intended analytical use rather than transferred directly from the Tuvalu experiment. Quality estimates derived from reviewed candidate pairs do not account for all genuine matches that the matching process may have failed to propose.

### 3. Assemble the prototype SPR

The script `spr data linkage.R` illustrates construction of the prototype register by linking the 2022 census to arrival records, adding birth records, and subsequently linking departure and death records.

It creates a prototype `spr.csv` file in `data/linked data/spr linkage/`, including source identifiers and other fields, as well as objects for investigating population changes over time.

For this experiment, unmatched arrivals were treated as additions to the population and births were added directly. Unmatched departures and deaths could not be attributed to a person in the register. These methodological assumptions, together with incomplete linkage, affect the interpretation of population estimates and are discussed in the final report. Check linkage integrity and uniqueness of generated identifiers before using an assembled dataset.

### 4. Link the 2022 and 2025 censuses

The script `fastlink census data linkage.R` investigates the linkage of the two censuses through five successive batches. The initial batch uses names, dates of birth, sex and location information. Subsequent batches test alternative combinations of fields on the remaining unmatched records, including removing name suffixes and using relationship to the household head, birthplace, GPS coordinates or enumeration area.

The analysis restricts the 2025 census to individuals born in or before 2022 to identify the relevant population for comparison.

Each batch requires its own completed review file. The script writes the final unmatched datasets and combines accepted linked identifiers in the R object `combined_linked_data`. Save this object separately in the secure working environment if it is needed for subsequent analysis.

### 5. Optional: prepare SQL Server tables

The script `create spr database.R` imports the prototype register and cleaned source datasets, connects to SQL Server via ODBC, creates a database if required and writes the datasets to separate tables.

Configure the database connection, authentication, encryption and file locations for the approved working environment. The supplied script uses local SQL Server settings, trusted authentication and `Encrypt = "no"`; these settings should be reviewed rather than carried over to another environment without assessment.

The `dbWriteTable()` calls use `overwrite = TRUE`, which can replace existing tables. Verify the destination and test the procedure in a suitable test database before writing to operational infrastructure. The script populates database tables but does not create a SQL Server `.bak` backup.

## Technical considerations for reuse

The scripts were developed for an exploratory record-linkage exercise involving iterative testing, clerical review and refinement of statistical methods. They reflect the data structures, research questions and technical environment of the original assignment.

### Methodological and data-quality considerations

Linkage outcomes depend on the quality, completeness and discriminatory power of the available identifying information. Inconsistent names and dates of birth, limited address information and the absence of a shared personal identifier were important challenges in the Tuvalu study. The use of multiple matching batches and manual review allowed alternative approaches to be investigated and uncertain matches to be assessed.

The appropriate choice of linkage variables, probability thresholds and treatment of unmatched records depends on the particular sources and purpose of the analysis. These choices should be reassessed when applying the methods elsewhere.

### Implementation checks

Moving from exploratory scripts to an operational statistical workflow would require additional testing, standardisation and validation. In addition to the environment and filename considerations described above, the following implementation details in the public scripts should be reviewed before execution or adaptation:

| Area | Consideration for reuse |
| --- | --- |
| Census-linkage batch configuration | The fourth census-linkage batch passes `thresholds` to `build_linked_unlinked()`, although the function does not accept this argument. Review the intended settings and remove or relocate the argument as appropriate. |
| Match probabilities and clerical overrides | In `build_linked_unlinked()`, posterior probabilities are attached to unlinked source records by position. If source rows and candidate pairs have different orderings, probabilities may become misaligned. Verify alignment using stable identifiers, check how review decisions override thresholds and validate the resulting linked and unlinked sets. |
| Register identifiers | The SPR script uses the same census-ID offset for newly generated identifiers for unmatched arrivals and births. Overlapping source IDs could therefore create collisions. Use a collision-free identifier strategy and check uniqueness during assembly. |
| Database table creation | The database script references `death_data_` when the object created earlier is `death_data`. Correct the object reference and test the database stage. |
| Parameters and reported results | Some settings in the public scripts differ from those described in the report. For example, the arrival-linkage threshold is `0` in the supplied SPR script, whereas the report describes `0.8` for the reported arrival analysis. Confirm the intended parameters, script versions and clerical review decisions before interpreting or comparing outputs. |

These observations concern the supplied scripts and are not a comprehensive code audit or an assessment of their effect on the original reported results. The public repository alone cannot establish whether or to what extent a potential implementation issue affected the original analysis.

Before adapting the code for operational use, test it with appropriate non-sensitive data and independently verify deduplication, candidate-pair alignment, review decisions, identifier uniqueness and reconciliation of final record counts. Document any changes to parameter settings, dependencies and processing logic.

## Outputs and interpretation

Depending on the scripts executed, the workflow can generate:

- Cleaned and deduplicated CSV files.
- Duplicate-review CSV files and Excel clerical review workbooks.
- Accepted linked identifier pairs and remaining unlinked records.
- A prototype SPR CSV file.
- SQL Server tables containing the register and associated source datasets.

These outputs may contain confidential personal information and should be managed accordingly. None of the original data outputs are included in the public repository.

The final report discusses the implications of inconsistent identifying information and incomplete linkage for maintaining a statistical population register. Its recommendations include improving the consistency of identifying variables, establishing an address reference, strengthening birth registration and developing local technical capacity.

An unmatched record does not necessarily represent a new individual or resident. Interpretation must take into account the coverage and quality of the source datasets, the likelihood of missed links and the purpose of the register.

## Report and acknowledgement

Letchford, N. (2025). *Data Integration for the Tuvalu Statistical Population Register: Final Report*. Pacific Community, Nouméa, New Caledonia. [Read the final report](https://spccfpstore1.blob.core.windows.net/digitallibrary-docs/files/01/01263827144a691bc20cc5088a00a524.pdf).

The study was undertaken in collaboration with Tuvalu's Central Statistics Division and SPC, with co-funding from the Government of Tuvalu and SPC through the World Bank-funded PacStat project. The report acknowledges the contributions of participating agencies, staff and the people of Tuvalu.

## Licence

No software licence has been specified in this repository. Confirm the applicable code ownership, permissions and licence terms before reusing or redistributing the code. The final report is subject to the terms stated in that publication.
