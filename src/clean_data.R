library(geosphere)
library(here)
library(hms)
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)

# load data
events <- read_excel(here("data", "02_interim", "TANGO_2_DATA_cleaned.xlsx"), sheet = "Events") %>%
  clean_names(case = "lower_camel") %>%
  select(!where(~ all(is.na(.))))  # remove empty columns

gear_protocol <- read_tsv(here("data", "01_raw", "gear_protocol.tsv")) 

samples <-  read_excel(here("data", "02_interim", "TANGO_2_DATA_cleaned.xlsx"), sheet = "Samples", col_types = "text") %>%
  clean_names(case = "lower_camel") %>%
  dplyr::filter(!if_all(everything(), is.na)) %>%  # remove empty rows 
  select(!where(~ all(is.na(.))))  # remove empty columns

# functions
# Convert degrees + decimal minutes to decimal degrees
ddm_to_dd <- function(x, assume_sign = 1) {
  x <- str_replace_all(x, ",", ".")
  
  degrees <- parse_number(str_extract(x, "-?\\d+(?=°)"))
  minutes <- parse_number(str_extract(x, "(?<=°)[0-9.]+"))
  
  direction <- case_when(
    str_detect(str_to_upper(x), "[SW]") ~ -1,
    str_detect(str_to_upper(x), "[NE]") ~  1,
    str_detect(x, "^\\s*-")             ~ -1,
    TRUE                                ~ assume_sign
  )
  
  direction * (abs(degrees) + minutes / 60)
}

# clean events
events_clean <- events %>%
  # filter out TOP events because it has been published in separate dataset.
  filter(!str_starts(eventId, "TOP")) %>%
  # create recordedBy field, concat collector1-4 separated by |
  unite(
    recordedBy,
    collector1:collector4,
    sep = " | ",
    na.rm = TRUE
  ) %>%
  # create recordedByID field, concat collector's Orcid separated by |
  unite(
    recordedByID,
    collector1Orcid:collector4Orcid,
    sep = " | ",
    na.rm = TRUE
  ) %>%
  rename(
    higherGeographyID = higherGeographyId
  ) %>%
  mutate(
    eventID = paste("TANGO2", eventId, sep = "_"),
    parentEventID = "https://www.wikidata.org/wiki/Q137398578",
    # add samplingProtocol based on gear_protocol.tsv which links gearType abbreviations to sampling protocols based on cruise report
    samplingProtocol = case_when(
      gearType %in% gear_protocol$abbreviation ~ gear_protocol$samplingProtocol[match(gearType, gear_protocol$abbreviation)],
      TRUE ~ NA_character_
    ),
    # round depth into integer
    minimumDepthInMeters = round(minimumDepthInMeters),
    maximumDepthInMeters = round(maximumDepthInMeters),
    lat_start = ddm_to_dd(latStartDecMin, assume_sign = -1),
    lat_stop  = ddm_to_dd(latStopDecMin,  assume_sign = -1),
    lon_start = ddm_to_dd(longStartDecMin, assume_sign = -1),
    lon_stop  = ddm_to_dd(longStopDecMin,  assume_sign = -1),
    
    decimalLatitude = round(case_when(
      !is.na(lat_start) & !is.na(lat_stop) ~
        (lat_start + lat_stop) / 2,
      TRUE ~ coalesce(lat_start, lat_stop)
    ), digits = 4),
    
    decimalLongitude = round(case_when(
      !is.na(lon_start) & !is.na(lon_stop) ~
        (lon_start + lon_stop) / 2,
      TRUE ~ coalesce(lon_start, lon_stop)
    ), digits = 4),
    
    # great-circle distance (meters)
    distance_m = distHaversine(
      cbind(lon_start, lat_start),
      cbind(lon_stop, lat_stop)
    ),
    # add buffer for cuim
    coordinateUncertaintyInMeters = round(distance_m / 2 + 2*10, 0),
    
    eventDate_chr = as.character(eventDate),
    eventTime_chr = as.character(eventTime),
    
    # Convert textual "NA" to actual NA
    eventTime_chr = na_if(eventTime_chr, "NA"),
    
    multi_day = str_detect(eventDate_chr, fixed("/")),
    
    start_text = case_when(
      multi_day ~ str_extract(eventDate_chr, "^[^/]+"),
      !is.na(eventTime_chr) ~ paste0(
        eventDate_chr,
        "T",
        str_extract(eventTime_chr, "^[^/]+")
      ),
      TRUE ~ NA_character_
    ),
    
    end_text = case_when(
      multi_day ~ str_extract(eventDate_chr, "[^/]+$"),
      !is.na(eventTime_chr) ~ paste0(
        eventDate_chr,
        "T",
        str_extract(eventTime_chr, "[^/]+$")
      ),
      TRUE ~ NA_character_
    ),
    
    # Change timezone offset from -03:00 to -0300
    start_text = str_replace(
      start_text,
      "([+-][0-9]{2}):([0-9]{2})$",
      "\\1\\2"
    ),
    
    end_text = str_replace(
      end_text,
      "([+-][0-9]{2}):([0-9]{2})$",
      "\\1\\2"
    ),
    
    start_datetime = as.POSIXct(
      start_text,
      format = "%Y-%m-%dT%H:%M%z",
      tz = "UTC"
    ),
    
    end_datetime = as.POSIXct(
      end_text,
      format = "%Y-%m-%dT%H:%M%z",
      tz = "UTC"
    ),
    
    sampleSizeValue = as.numeric(
      difftime(end_datetime, start_datetime, units = "mins")
    ),
    sampleSizeUnit = "minutes"
  ) %>%
  relocate(eventID, .before = 1) %>%
  add_row(
    eventID = "https://www.wikidata.org/wiki/Q137398578",
    eventDate = "2024-02-05/2024-03-08"
  ) %>%
  select(-eventId)
  

# samples
samples_clean <- samples %>%
  rowwise() %>%
  mutate(
    eventID = paste("TANGO2", eventId, sep = "_"),
    recordedBy = paste(
      na.omit(c_across(starts_with("recordedBy") & !starts_with("recordedByID"))),
      collapse = " | "
    ),
    recordedBy = if_else(recordedBy == "", NA_character_, recordedBy),
    recordedByID = paste(
      na.omit(c_across(starts_with("recordedByID"))),
      collapse = " | "
    ),
    recordedByID = if_else(recordedByID == "", NA_character_, recordedByID),
    identifiedBy = paste(
      na.omit(c_across(starts_with("identifiedBy") & !starts_with("identifiedByID"))),
      collapse = " | "
    ),
    identifiedBy = if_else(identifiedBy == "", NA_character_, identifiedBy),
    identifiedByID = paste(
      na.omit(c_across(starts_with("identifiedByID"))),
      collapse = " | "
    ),
    identifiedByID = if_else(identifiedByID == "", NA_character_, identifiedByID)
  ) %>%
  ungroup() 


# occurrence
occ <- samples_clean %>%
  filter(sampleType == "Biota" | sampleType == "biota") %>%
  mutate(
    basisOfRecord = "PreservedSpecimen",
    occurrenceStatus = "detected",
    occurrenceRemarks = remark,
    occurrenceID = paste0(eventID, "_", sampleId, "_", parentSampleId, "_", preservedIn1, "_", container1)
    ) %>%
  relocate(occurrenceID, .before = 1) %>%
  select(-eventId)

# parentSampleID makes no sense, comes from sampleID from another event.
non_unique_occ <- occ %>% add_count(occurrenceID) %>% filter(n > 1) 

# check if there is eventID in occ that is not in events_clean
missing_eventID <- occ %>%
  distinct(eventID) %>%
  anti_join(
    events_clean %>% distinct(eventID),
    by = "eventID"
  )

# save cleaned data
write_tsv(events_clean, here("data", "03_output", "tango_2_event.tsv"), na = "")
write_tsv(occ, here("data", "03_output", "tango_2_occurrence.tsv"), na = "")


