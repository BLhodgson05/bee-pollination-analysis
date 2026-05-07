# R_merge.R
# RStudio user: Ben Hodgson (Student number: 202317270)
# Date last worked on:
# 7/5/2026

# Code to merge all raw city-year CSV datasets

# Make these packages are their associated functions available for use in this 
# script: 
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)


### -------------------------------------
# 1) Set paths
### -------------------------------------

# Raw city-year CSV files
raw_data_path <- "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/raw_data/city-year downloads"

# Output folders
dat_all_output_path <- "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/raw_data/dat_all_merged"

city_output_path <- "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/raw_data/dat_by_city"

year_output_path <- "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/raw_data/dat_by_year"

# Create folders if missing
dir.create(dat_all_output_path, recursive = TRUE, showWarnings = FALSE)

dir.create(city_output_path, recursive = TRUE, showWarnings = FALSE)

dir.create(year_output_path, recursive = TRUE, showWarnings = FALSE)


### -------------------------------------
# 2) List raw CSV files
### -------------------------------------

raw_files <- list.files(
  path = raw_data_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(raw_files) == 0) {
  stop("No .csv files found in raw_data_path.")
}


### -------------------------------------
# 3) Function to read and label files
### -------------------------------------

read_city_year_file <- function(file) {
  
  file_name <- basename(file)
  file_stub <- tools::file_path_sans_ext(file_name)
  
  # Extract year from filenames like london_25
  year_short <- str_extract(file_stub, "[0-9]{2}$")
  
  if (is.na(year_short)) {
    stop(paste("Could not extract year from:", file_name))
  }
  
  year_value <- as.integer(paste0("20", year_short))
  
  # Extract city name
  city_value <- file_stub %>%
    str_remove("_[0-9]{2}$") %>%
    str_replace_all("_", " ") %>%
    str_replace_all("-", " ") %>%
    str_squish() %>%
    str_to_lower()
  
  # Read CSV
  read_csv(
    file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    clean_names() %>%
    mutate(
      city = city_value,
      year = year_value,
      source_file = file_name
    )
}


### -------------------------------------
# 4) Merge all datasets
### -------------------------------------

dat_all <- map_dfr(raw_files, read_city_year_file)

if (nrow(dat_all) == 0) {
  stop("dat_all has 0 rows after merging.")
}


### -------------------------------------
# 5) Export dat_all
### -------------------------------------

write_csv(
  dat_all,
  file.path(dat_all_output_path, "dat_all.csv")
)


### -------------------------------------
# 6) Export city datasets
### -------------------------------------

city_names <- sort(unique(dat_all$city))

for (city_i in city_names) {
  
  dat_city <- dat_all %>%
    filter(city == city_i)
  
  city_file_name <- paste0(
    "dat_",
    str_replace_all(city_i, "[^a-z0-9]+", "_"),
    ".csv"
  )
  
  write_csv(
    dat_city,
    file.path(city_output_path, city_file_name)
  )
}


### -------------------------------------
# 7) Export year datasets
### -------------------------------------

years <- sort(unique(dat_all$year))

for (year_i in years) {
  
  dat_year <- dat_all %>%
    filter(year == year_i)
  
  year_file_name <- paste0("dat_", year_i, ".csv")
  
  write_csv(
    dat_year,
    file.path(year_output_path, year_file_name)
  )
}