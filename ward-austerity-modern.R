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

# Helper to generate monthly date sequences
timeVector <- function(start_date, end_date) {
  seq(as.Date(start_date), as.Date(end_date), by = "month")
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
if(file.exists("shapefiles/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp")) {
  WARD17.sf <- st_read("shapefiles/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.shp")
  WARD17 <- data.table(WARD17.sf)
} else {
  cat("Error: 2017 ward boundaries shapefile not found\n")
  stop("Required shapefile missing")
}

# Load EU referendum data
cat("Loading EU referendum data...\n")
if(file.exists("Ward EURef/EU-referendum-result-data.csv")) {
  EUREF <- data.table(read.csv("Ward EURef/EU-referendum-result-data.csv"))
  # The EU referendum data is at local authority level, not ward level
  # We'll skip this merge for now as the ward boundaries don't have LAD17CD
  cat("Loaded EU referendum data for", nrow(EUREF), "local authorities (skipping merge - different level)\n")
} else {
  cat("Warning: EU referendum data not found\n")
}

if(file.exists("Ward EURef/ward-results.csv")) {
  REF <- data.table(read.csv("Ward EURef/ward-results.csv"))
  REF <- REF[WardCode != ""]
  # Use the correct column names from ward-results.csv
  REF <- REF[, list(wd17cd = WardCode, Ward_Remain = Remain, Ward_Leave = Leave, 
                    Ward_RemainPct = Remain., Ward_LeavePct = Leave.)]
  WARD17 <- join(WARD17, REF, by = "wd17cd")
  cat("Loaded ward-level referendum data for", nrow(REF), "wards\n")
} else {
  cat("Warning: Ward-level referendum data not found\n")
}

# Load population data
cat("Loading population data...\n")
if(file.exists("Ward Characteristics/WARD_POP_2015.csv")) {
  POP <- data.table(read.csv("Ward Characteristics/WARD_POP_2015.csv"))
  POP[, All.Ages := as.numeric(gsub(",", "", All.Ages))]
  POP.WARD <- POP[, list(wd15cd = Ward, POP2015 = as.numeric(gsub(",", "", All.Ages)))][wd15cd != ""]
  WARD17 <- join(WARD17, POP.WARD[, list(wd15cd, POP2015)], by = "wd17cd")
  cat("Loaded population data for", nrow(POP.WARD), "wards\n")
} else if(file.exists("Ward Characteristics/WARD_POP_2015.csv")) {
  POP <- data.table(read.csv("Ward Characteristics/WARD_POP_2015.csv"))
  POP[, All.Ages := as.numeric(gsub(",", "", All.Ages))]
  POP.WARD <- POP[, list(wd15cd = Ward, POP2015 = as.numeric(gsub(",", "", All.Ages)))][wd15cd != ""]
  WARD17 <- join(WARD17, POP.WARD[, list(wd15cd, POP2015)], by = "wd17cd")
  cat("Loaded population data for", nrow(POP.WARD), "wards\n")
} else {
  cat("Warning: Population data not found\n")
}


# Additional spatial crosswalks translated from ward-austerity.R
cat("Mapping 2011 wards to 2017 wards...\n")
if (file.exists("Ward Level Local Election Results/shp/infuse_ward_lyr_2011_clipped/infuse_ward_lyr_2011_clipped.shp")) {
  SHP11.sf <- st_read("Ward Level Local Election Results/shp/infuse_ward_lyr_2011_clipped/infuse_ward_lyr_2011_clipped.shp")
  CENT11.sf <- st_centroid(SHP11.sf)
  CENT11.join <- st_join(CENT11.sf, WARD17.sf["wd17cd"], left = TRUE)
  MAPPER <- data.table(st_drop_geometry(CENT11.join))[, .(wd11cd = geo_code, wd17cd)]
  rm(SHP11.sf, CENT11.sf, CENT11.join)
} else {
  cat("Warning: 2011 ward shapefile not found\n")
}

# Load and merge census characteristics
cat("Loading additional census data...\n")
if (dir.exists("Ward Characteristics/Census 2011") && exists("MAPPER")) {
  ffs <- list.files("Ward Characteristics/Census 2011")
  LABEL <- NULL
  for (ff in ffs) {
    path <- file.path("Ward Characteristics/Census 2011", ff, "")
    FILE <- readFiles(path, collate = "list", fname = FALSE)
    if (length(FILE) >= 2) {
      FILE[[2]][, label := paste("label variable ", ColumnVariableCode, " \"", ff, " ",
                                 ColumnVariableMeasurementUnit, " ", ColumnVariableDescription, "\"", sep = "")]
      LABEL <- c(LABEL, FILE[[2]]$label)
    }
    FILE[[1]] <- join(FILE[[1]], MAPPER[, .(GeographyCode = wd11cd, wd17cd)], by = "GeographyCode")
    FILE[[1]] <- FILE[[1]][!is.na(wd17cd)]
    num_cols <- setdiff(names(FILE[[1]]), c("GeographyCode", "wd17cd"))
    FILE[[1]][, (num_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = num_cols]
	FILE[[1]][, GeographyCode := NULL]

    WARD17 <- join(WARD17, DTUniqueBy(FILE[[1]], "wd17cd"), by = "wd17cd")

  }
  if (length(LABEL)) saveRDS(LABEL, file = "intermediate/census_labels.rds")
} else {
  cat("Warning: Census data directory not found or mapper missing\n")
}


cat("Mapping ward centroids to MSOA 2011 boundaries...\n")
if (file.exists("shapefiles/Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales/Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales.shp")) {
  MSOA.sf <- st_read("shapefiles/Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales/Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales.shp")
  WARD17.centroids <- st_centroid(WARD17.sf)
  WARD17.centroids <- st_join(WARD17.centroids, MSOA.sf[, c("msoa11nm", "msoa11cd")], left = TRUE)
  WARD17_CENT <- data.table(
    wd17cd = WARD17.sf$wd17cd,
    x = st_coordinates(WARD17.centroids)[, 1],
    y = st_coordinates(WARD17.centroids)[, 2],
    msoa11nm = st_drop_geometry(WARD17.centroids)$msoa11nm,
    msoa11cd = st_drop_geometry(WARD17.centroids)$msoa11cd
  )
  rm(MSOA.sf, WARD17.centroids)
} else {
  cat("Warning: MSOA 2011 shapefile not found\n")
}

cat("Mapping LSOA 2001 centroids to 2017 wards...\n")
if (file.exists("shapefiles/Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales.shp")) {
  LSOA.sf <- st_read("shapefiles/Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales.shp")
  LSOA.centroids <- st_centroid(LSOA.sf)
  LSOA.centroids <- st_join(LSOA.centroids, WARD17.sf["wd17cd"], left = TRUE)
  LSOA_CENT <- data.table(
    lsoa01nm = LSOA.sf$lsoa01nm,
    x = st_coordinates(LSOA.centroids)[, 1],
    y = st_coordinates(LSOA.centroids)[, 2],
    wd17cd = st_drop_geometry(LSOA.centroids)$wd17cd
  )
  rm(LSOA.sf, LSOA.centroids)
} else {
  cat("Warning: LSOA 2001 shapefile not found\n")
}

# ----------------------------------------------------------------------------
#  Ward level sanctions and claimant data
# ----------------------------------------------------------------------------

if (!dir.exists("intermediate")) dir.create("intermediate")

cat("Processing sanctions data...\n")
types <- c("WP","JSA","ESA","UC","BTAX","PIP")
OUT <- list()

for(type in types) {
  ff <- list.files("SANCTIONS", pattern = type, full.names = TRUE)
  if(length(ff) == 0) next
  JSA <- data.table(read.csv(ff[1]))
  if("OA" %in% names(JSA)) {
    JSA[OA == "", OA := NA]
    JSA[, OA := na.locf(OA)]
  }
  JSA.LONG <- melt(JSA, id.vars = c("OA","Type"))
  JSA.LONG[, value := as.numeric(value)]
  setnames(JSA.LONG, "variable", "date")

  if(type %in% c("WP","UC")) {
    JSA.LONG[, date := dmy(paste("01", gsub("\\.", " ", date)))]
    JSA.LONG[, year := year(date)]
    JSA.LONG[, month := month(date)]
    JSA.LONG[, quarter := quarter(date)]
  } else {
    JSA.LONG[, year := as.numeric(substr(date,2,5))]
    JSA.LONG[, month := as.numeric(substr(date,6,7))]
    JSA.LONG[, timevec := dmy(paste("01", month, year, sep = "-"))]
    JSA.LONG[, quarter := quarter(timevec)]
    JSA.LONG[, timevec := NULL]
  }

  JSA.LONG <- JSA.LONG[!is.na(year)]
  JSA.LONG[, date := NULL]
  JSA.LONG<-JSA.LONG[, list(value=sum(value,na.rm=TRUE)), by=c("OA","Type","year","quarter")]
  if(exists("WARD17_CENT")) {
    JSA.LONG <- join(JSA.LONG, WARD17_CENT[, list(OA = msoa11nm, wd17cd, msoa11cd)], by = "OA")
  }

  JSA.LONG <- JSA.LONG[!is.na(wd17cd)]
  JSA.LONG <- JSA.LONG[, .(value = mean(value, na.rm = TRUE)), by = .(wd17cd, Type, year, quarter)]
  JSA.LONG[, measure := type]
  OUT[[type]] <- JSA.LONG
  saveRDS(JSA.LONG, file = file.path("intermediate", paste0("sanctions_", tolower(type), ".rds")))
}

cat("Reshaping sanctions data...\n")
for(measure in names(OUT)) {
  tts <- OUT[[measure]][, unique(Type)]
  for(tt in tts) {
    TEMP <- OUT[[measure]][Type == tt]
    TEMP[, time := paste(year, "q", quarter, sep = "")]
    TEMP.WIDE <- dcast(TEMP[, .(wd17cd, time, value)], wd17cd ~ time)
    tt_clean <- str_trim(gsub("\\(.*\\)", "", tt))
    tt_clean <- gsub("Decision not to apply a sanction", "No Sanction", tt_clean)
    tt_clean <- gsub("Decision to apply a sanction", "Sanction", tt_clean)
    varname <- paste(measure, gsub("[^A-z0-9]", "", tt_clean), sep = "_")
    value_cols <- setdiff(names(TEMP.WIDE), "wd17cd")
    setnames(TEMP.WIDE, value_cols, paste0(varname, value_cols))
    WARD17 <- join(WARD17, TEMP.WIDE, by = "wd17cd")
    saveRDS(TEMP.WIDE, file = file.path("intermediate", paste0("sanctions_", tolower(measure), "_", gsub("[^A-z0-9]", "", tt_clean), ".rds")))
  }
}

cat("Processing claimant data...\n")
ffs <- list.files("Claimants", full.names = TRUE)
CLAIM <- data.table()

for(ff in ffs) {
  TMP <- readLines(ff)
  ITEMS <- grep("Item Name  :", TMP)
  ITEMS.LABEL <- TMP[ITEMS]
  for(it in seq_along(ITEMS)) {
    rows <- if(it == length(ITEMS)) -1 else ITEMS[it+1] - ITEMS[it]
    TT <- read.csv(ff, skip = ITEMS[it] + 1, nrows = rows)
    TT.LONG <- data.table(melt(TT, id.vars = c("X2003.CAS.ward", "mnemonic")))
    TT.LONG[, variable := paste("01.", variable)]
    TT.LONG[, date := dmy(variable)]
    TT.LONG[, list := ITEMS.LABEL[it]]
    CLAIM <- rbind(CLAIM, TT.LONG)
  }
}

CLAIM[, list := gsub("\"Item Name  :\",|\"", "", list)]
CLAIM <- unique(CLAIM)
saveRDS(CLAIM, file = "intermediate/claim_raw.rds")

cat("Mapping CAS wards to 2017 wards...\n")
if (file.exists("shapefiles/CASW/UK_caswa_2001NAD1936.shp")) {
  CASWARD.sf <- st_read("shapefiles/CASW/UK_caswa_2001NAD1936.shp")
  CASWARD.cent <- st_centroid(CASWARD.sf)
  CASWARD.cent <- st_join(CASWARD.cent, WARD17.sf["wd17cd"], left = TRUE)
  CASWARD_CENT <- data.table(
    mnemonic = CASWARD.sf$ons_label,
    wd17cd = st_drop_geometry(CASWARD.cent)$wd17cd
  )
  MAPPER <- unique(CASWARD_CENT[!is.na(wd17cd)], by = "mnemonic")
  CLAIM.MERGE <- join(CLAIM, MAPPER, by = "mnemonic")
  CLAIM.MERGE <- CLAIM.MERGE[!is.na(wd17cd)]
  CLAIM.MERGE[, quarter := quarter(date)]
  CLAIM.MERGE[, year := year(date)]
  CLAIM.MERGE <- CLAIM.MERGE[, .(value = mean(as.numeric(as.character(value)), na.rm = TRUE)), by = .(wd17cd, year, quarter, list)]
  saveRDS(CLAIM.MERGE, file = "intermediate/claim_merged.rds")
  lists <- unique(CLAIM.MERGE$list)
  for(li in lists) {
    TEMP <- CLAIM.MERGE[list == li & year >= 2007][order(year, quarter)]
    TEMP[, time := paste(year, "q", quarter, sep = "")]
    TEMP.WIDE <- dcast(TEMP[, .(wd17cd, time, value)], wd17cd ~ time)
    tt <- str_trim(gsub("statistical group - ", "", li))
    varname <- paste("CLAIM", gsub("[^A-z0-9]", "", tt), sep = "_")
    setnames(TEMP.WIDE, setdiff(names(TEMP.WIDE), "wd17cd"), paste0(varname, ".", setdiff(names(TEMP.WIDE), "wd17cd")))
    WARD17 <- join(WARD17, TEMP.WIDE, by = "wd17cd")
    saveRDS(TEMP.WIDE, file = file.path("intermediate", paste0("claim_", gsub("[^A-z0-9]", "", tt), ".rds")))
  }
} else {
  cat("Warning: CAS ward shapefile not found, claimant data skipped\n")
}

cat("Processing housing benefit cuts...\n")
if (file.exists("Ward Level Austerity/HB_COUNT.csv")) {
  HBC <- data.table(read.csv("Ward Level Austerity/HB_COUNT.csv"))
  HBC.LONG <- melt(HBC, id.vars = "OA")
  HBC.LONG <- HBC.LONG[variable != "Month" & value != ".." & !is.na(value)]
  HBC.LONG[, date := str_extract(variable, "[0-9]{6}")]
  HBC.LONG <- HBC.LONG[!grepl("Not avail|Total", OA)]
  HBC.LONG[, variable := NULL]
  setnames(HBC.LONG, "value", "housing_ben_rec_count")
  HBC.LONG[, year := as.numeric(substr(date,1,4))]
  HBC.LONG[, month := as.numeric(substr(date,5,6))]
  HBC.LONG <- join(HBC.LONG, LSOA_CENT[, .(OA = lsoa01nm, wd17cd)], by = "OA", all.x = TRUE)
  HBC.LONG <- HBC.LONG[!is.na(wd17cd)]
  HBC.LONG[, housing_ben_rec_count := as.numeric(as.character(housing_ben_rec_count))]
  HBC.WARD <- HBC.LONG[, .(housing_ben_rec_count = sum(housing_ben_rec_count)), by = .(wd17cd, year, month)]
  saveRDS(HBC.WARD, file = "intermediate/hbc_ward.rds")
} else { cat("Warning: housing benefit cuts file not found\n") }

cat("Processing bedroom tax cases...\n")
if (file.exists("Ward Level Austerity/BEDROOM_TAX_CASES.csv")) {
  BTX <- data.table(read.csv("Ward Level Austerity/BEDROOM_TAX_CASES.csv"))
  BTX[Type == "", Type := NA]
  BTX[, Type := na.locf(Type)]
  BTX <- BTX[Type == "Reduction applied"]
  BTX[, Type := NULL]
  BTX.LONG <- melt(BTX, id.vars = "OA")
  BTX.LONG <- BTX.LONG[!(variable %in% c("X","Month","X.1")) & !is.na(value)]
  BTX.LONG[, value := as.numeric(value)]
  setnames(BTX.LONG, "variable", "date")
  BTX.LONG[, year := as.numeric(substr(date,2,5))]
  BTX.LONG[, month := as.numeric(substr(date,6,7))]
  BTX.LONG[, date := NULL]
  BTX.LONG <- merge(BTX.LONG, LSOA_CENT[, .(OA = lsoa01nm, wd17cd)], by = "OA", all.x = TRUE)
  BTX.LONG <- BTX.LONG[!is.na(wd17cd)]
  BTX.WARD <- BTX.LONG[, .(btx_cases = sum(value)), by = .(wd17cd, year, month)]
  saveRDS(BTX.WARD, file = "intermediate/btx_ward.rds")
} else { cat("Warning: bedroom tax file not found\n") }

cat("Processing PIP registrations...\n")
if (file.exists("Ward Level Austerity/PIP Registrations.csv")) {
  PIP <- data.table(read.csv("Ward Level Austerity/PIP Registrations.csv"))
  PIP[OA == "", OA := NA]
  PIP[, OA := na.locf(OA)]
  PIP.LONG <- melt(PIP, id.vars = c("OA","Type"))
  PIP.LONG[, value := as.numeric(value)]
  setnames(PIP.LONG, "variable", "date")
  PIP.LONG[, year := as.numeric(substr(date,2,5))]
  PIP.LONG[, month := as.numeric(substr(date,6,7))]
  PIP.LONG <- PIP.LONG[date != "X" & !is.na(year)]
  PIP.LONG[, date := NULL]
  PIP.LONG <- PIP.LONG[!is.na(value)]
  PIP.LONG <- merge(PIP.LONG, LSOA_CENT[, .(OA = lsoa01nm, wd17cd)], by = "OA", all.x = TRUE)
  PIP.LONG <- PIP.LONG[!is.na(wd17cd)]
  PIP.WARD <- PIP.LONG[, .(pipcases = sum(value)), by = .(wd17cd, Type, year, month)]
  saveRDS(PIP.WARD, file = "intermediate/pip_ward.rds")
} else { cat("Warning: PIP registrations file not found\n") }

cat("Processing JSA sanctions...\n")
if (file.exists("Ward Level Austerity/JSA Sanctions high frequency around ref.csv")) {
  JSA <- data.table(read.csv("Ward Level Austerity/JSA Sanctions high frequency around ref.csv"))
  JSA[OA == "", OA := NA]
  JSA[, OA := na.locf(OA)]
  JSA.LONG <- melt(JSA, id.vars = c("OA","Type"))
  JSA.LONG[, value := as.numeric(value)]
  setnames(JSA.LONG, "variable", "date")
  JSA.LONG[, year := as.numeric(substr(date,2,5))]
  JSA.LONG[, month := as.numeric(substr(date,6,7))]
  JSA.LONG <- JSA.LONG[date != "X" & !is.na(year)]
  JSA.LONG[, date := NULL]
  JSA.LONG <- JSA.LONG[!is.na(value)]
  JSA.LONG <- merge(JSA.LONG, LSOA_CENT[, .(OA = lsoa01nm, wd17cd)], by = "OA", all.x = TRUE)
  JSA.LONG <- JSA.LONG[!is.na(wd17cd)]
  JSA.WARD <- JSA.LONG[, .(jsasanctioncases = sum(value)), by = .(wd17cd, Type, year, month)]
  saveRDS(JSA.WARD, file = "intermediate/jsa_ward.rds")
} else { cat("Warning: JSA sanctions file not found\n") }

cat("Building ward panel...\n")
WARD.PAN <- rbindlist(lapply(WARD17$wd17cd, function(x) data.table(wd17cd = x, timevec = timeVector("2007-01-12", "2017-12-31"))))
WARD.PAN[, year := year(timevec)]
WARD.PAN[, month := month(timevec)]

if (exists("HBC.WARD")) WARD.PAN <- merge(WARD.PAN, HBC.WARD, by = c("wd17cd","year","month"), all.x = TRUE)
if (exists("BTX.WARD")) WARD.PAN <- merge(WARD.PAN, BTX.WARD, by = c("wd17cd","year","month"), all.x = TRUE)
if (exists("PIP.WARD")) {
  WARD.PAN <- merge(WARD.PAN, PIP.WARD[Type == "Total", .(wd17cd, year, month, pip_total_registrations = pipcases)], by = c("wd17cd","year","month"), all.x = TRUE)
  WARD.PAN <- merge(WARD.PAN, PIP.WARD[Type == "Reassessment", .(wd17cd, year, month, pip_reassessment_registrations = pipcases)], by = c("wd17cd","year","month"), all.x = TRUE)
}
if (exists("JSA.WARD")) {
  WARD.PAN <- merge(WARD.PAN, JSA.WARD[Type == "Decision to apply a sanction (adverse) (IX)", .(wd17cd, year, month, jsa_sanctions_adverse = jsasanctioncases)], by = c("wd17cd","year","month"), all.x = TRUE)
  WARD.PAN <- merge(WARD.PAN, JSA.WARD[Type == "Decision not to apply a sanction (non-adverse) (VIII)", .(wd17cd, year, month, jsa_sanctions_nonadverse = jsasanctioncases)], by = c("wd17cd","year","month"), all.x = TRUE)
  WARD.PAN <- merge(WARD.PAN, JSA.WARD[Type == "Total", .(wd17cd, year, month, jsa_sanctions_total = jsasanctioncases)], by = c("wd17cd","year","month"), all.x = TRUE)
}

saveRDS(WARD.PAN, file = "intermediate/ward_panel.rds")

WARD.PAN[is.na(WARD.PAN)] <- 0
AROUNDREF <- WARD.PAN[year >= 2012 & year <= 2017]
AROUNDREF[month < 10, time := as.numeric(paste0(year, "0", month))]
AROUNDREF[month >= 10, time := as.numeric(paste0(year, month))]

vars <- setdiff(names(AROUNDREF), c("wd17cd", "timevec", "year", "month", "time"))
for(vv in vars) {
  TEMP <- dcast(AROUNDREF[, .(wd17cd, time, value = get(vv))], wd17cd ~ time)
  setnames(TEMP, setdiff(names(TEMP), "wd17cd"), paste0(vv, "_", setdiff(names(TEMP), "wd17cd")))
  WARD17 <- merge(WARD17, TEMP, by = "wd17cd", all.x = TRUE)
  saveRDS(TEMP, file = file.path("intermediate", paste0("aroundref_", vv, ".rds")))
}

cat("Saving WARD_LEVEL_REF.dta...\n")
WARD17_SAVE2 <- WARD17[, !"geometry"]
write_dta(WARD17_SAVE2, "WARD_LEVEL_REF.dta")


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
setnames(WARD17_SAVE, names(WARD17_SAVE), janitor::make_clean_names(names(WARD17_SAVE)))
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
