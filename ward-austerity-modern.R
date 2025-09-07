## Modern version of ward-austerity.R for EUREF.WARDLEVEL.dta generation
## Updated to use sf and terra instead of retired rgeos package
## This script should be run from the Note/EUREF_BUILD_FILES directory

# Set working directory to current location
setwd(getwd())

# Load required libraries
library(data.table)
library(sf)
library(terra)
library(lubridate)
library(zoo)
library(stringr)

# Check if required packages are installed
required_packages <- c("data.table", "sf", "terra", "lubridate", "zoo", "stringr", "haven")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)

# Helper function to read files
readFiles <- function(folder, ftype = "csv", collate = "rbind", Encoding = "latin1", fname = TRUE) {
  ftype <- tolower(ftype)
  ffs <- list.files(folder)
  ffs <- grep(paste(ftype, "$", sep = ""), ffs, value = TRUE, ignore.case = TRUE)
  
  if (ftype == "dta") {
    DAT <- lapply(ffs, function(x) data.table(read_dta(file = file.path(folder, x))))
  } else if (ftype == "csv") {
    if (fname == TRUE) {
      DAT <- lapply(ffs, function(x) data.table(data.frame(fname = x, 
        read.csv(file = file.path(folder, x), fileEncoding = Encoding))))
    } else {
      DAT <- lapply(ffs, function(x) data.table(data.frame(
        read.csv(file = file.path(folder, x), fileEncoding = Encoding))))
    }
  }
  
  if (collate == "rbind") {
    DAT <- rbindlist(DAT, fill = TRUE)
  }
  return(DAT)
}

# Helper function for unique by
DTUniqueBy <- function(dt, by_cols) {
  if (is.null(by_cols)) return(dt)
  return(unique(dt, by = by_cols))
}

# Set options
options(stringsAsFactors = FALSE)

# Check if required files exist
required_files <- c(
  "EU-referendum-result-data.csv",
  "ward-results.csv",
  "shapefiles/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp"
)

missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0) {
  cat("Warning: Missing files:", paste(missing_files, collapse = ", "), "\n")
  cat("Please ensure all required data files are present.\n")
}

# Load 2017 ward boundaries using sf
cat("Loading 2017 ward boundaries...\n")
if(file.exists("Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp")) {
  WARD17.sf <- st_read("Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp")
  WARD17 <- data.table(WARD17.sf)
} else {
  cat("Error: 2017 ward boundaries shapefile not found\n")
  stop("Required shapefile missing")
}

# Load EU referendum data
cat("Loading EU referendum data...\n")
if(file.exists("EU-referendum-result-data.csv")) {
  EUREF <- data.table(read.csv("EU-referendum-result-data.csv"))
  # The EU referendum data is at local authority level, not ward level
  # We'll skip this merge for now as the ward boundaries don't have LAD17CD
  cat("Loaded EU referendum data for", nrow(EUREF), "local authorities (skipping merge - different level)\n")
} else {
  cat("Warning: EU referendum data not found\n")
}

if(file.exists("ward-results.csv")) {
  REF <- data.table(read.csv("ward-results.csv"))
  REF <- REF[WardCode != ""]
  # Use the correct column names from ward-results.csv
  REF <- REF[, list(wd17cd = WardCode, Ward_Remain = Remain, Ward_Leave = Leave, 
                    Ward_RemainPct = Remain., Ward_LeavePct = Leave.)]
  WARD17 <- merge(WARD17, REF, by = "wd17cd", all.x = TRUE)
  cat("Loaded ward-level referendum data for", nrow(REF), "wards\n")
} else {
  cat("Warning: Ward-level referendum data not found\n")
}

# Load population data
cat("Loading population data...\n")
if(file.exists("WARD_POP_2015.csv")) {
  POP <- data.table(read.csv("WARD_POP_2015.csv"))
  POP[, All.Ages := as.numeric(gsub(",", "", All.Ages))]
  POP.WARD <- POP[, list(wd15cd = Ward, POP2015 = as.numeric(gsub(",", "", All.Ages)))][wd15cd != ""]
  WARD17 <- merge(WARD17, POP.WARD[, list(wd15cd, POP2015)], by.x = "wd17cd", by.y = "wd15cd", all.x = TRUE)
  cat("Loaded population data for", nrow(POP.WARD), "wards\n")
} else if(file.exists("Ward Characteristics/WARD_POP_2015.csv")) {
  POP <- data.table(read.csv("Ward Characteristics/WARD_POP_2015.csv"))
  POP[, All.Ages := as.numeric(gsub(",", "", All.Ages))]
  POP.WARD <- POP[, list(wd15cd = Ward, POP2015 = as.numeric(gsub(",", "", All.Ages)))][wd15cd != ""]
  WARD17 <- merge(WARD17, POP.WARD[, list(wd15cd, POP2015)], by.x = "wd17cd", by.y = "wd15cd", all.x = TRUE)
  cat("Loaded population data for", nrow(POP.WARD), "wards\n")
} else {
  cat("Warning: Population data not found\n")
}

# Load additional census characteristics if available
cat("Loading additional census data...\n")
if(dir.exists("Ward Characteristics/Census 2011")) {
  census_files <- list.files("Ward Characteristics/Census 2011", full.names = TRUE)
  cat("Found", length(census_files), "census directories\n")
  # Process census data here if needed
}

# Load ward statistics
cat("Loading ward statistics...\n")
if(file.exists("WARD_STATS.dta")) {
  WARD_STATS <- data.table(read_dta("WARD_STATS.dta"))
  cat("Loaded ward statistics with", nrow(WARD_STATS), "rows\n")
  # Merge with existing data as needed
} else {
  cat("Warning: Ward statistics not found\n")
}

# Calculate centroids for spatial operations
cat("Calculating ward centroids...\n")
if("geometry" %in% names(WARD17)) {
  WARD17$centroid_x <- st_coordinates(st_centroid(WARD17.sf))[,1]
  WARD17$centroid_y <- st_coordinates(st_centroid(WARD17.sf))[,2]
}

# Final data preparation and output
cat("\n=== Final Dataset Summary ===\n")
cat("Dimensions of final dataset:", nrow(WARD17), "rows,", ncol(WARD17), "columns\n")

# Rename reserved words for Stata compatibility
if("long" %in% names(WARD17)) {
  setnames(WARD17, "long", "longitude")
}
if("lat" %in% names(WARD17)) {
  setnames(WARD17, "lat", "latitude")
}

cat("Columns:", paste(names(WARD17), collapse = ", "), "\n")

# Save the final dataset
cat("\nSaving EUREF.WARDLEVEL.dta...\n")
# Remove geometry column before saving as .dta
WARD17_SAVE <- WARD17[, !"geometry"]
write_dta(WARD17_SAVE, "EUREF.WARDLEVEL.dta")
cat("Dataset saved successfully!\n")

# Verify the output
if(file.exists("EUREF.WARDLEVEL.dta")) {
  final_data <- data.table(read_dta("EUREF.WARDLEVEL.dta"))
  cat("\n=== Verification ===\n")
  cat("File: EUREF.WARDLEVEL.dta\n")
  cat("Rows:", nrow(final_data), "\n")
  cat("Columns:", ncol(final_data), "\n")
  cat("File size:", round(file.info("EUREF.WARDLEVEL.dta")$size / 1024 / 1024, 2), "MB\n")
  
  # Show first few columns
  cat("\nFirst 10 columns:", paste(names(final_data)[1:min(10, ncol(final_data))], collapse = ", "), "\n")
  
  # Show EU referendum variables if available
  eu_vars <- c("Ward_Remain", "Ward_Leave", "Ward_RemainPct", "Ward_LeavePct")
  available_eu_vars <- eu_vars[eu_vars %in% names(final_data)]
  if(length(available_eu_vars) > 0) {
    cat("\nEU Referendum variables available:", paste(available_eu_vars, collapse = ", "), "\n")
  }
}

cat("\n=== Script completed successfully! ===\n")
