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

samples <-  read_excel(here("data", "02_interim", "TANGO_2_DATA_cleaned.xlsx"), sheet = "Samples") %>%
  clean_names(case = "lower_camel") %>%
  dplyr::filter(!if_all(everything(), is.na)) %>%  # remove empty rows 
  select(!where(~ all(is.na(.))))  # remove empty columns

# clean events
# todo: not yet done. duration is fucked up
events_clean <- events %>%
  mutate(
    parentEventID = "https://www.wikidata.org/wiki/Q137398578",
    # add samplingProtocol based on gear_protocol.tsv which links gearType abbreviations to sampling protocols based on cruise report
    samplingProtocol = case_when(
      gearType %in% gear_protocol$abbreviation ~ gear_protocol$samplingProtocol[match(gearType, gear_protocol$abbreviation)],
      TRUE ~ NA_character_
    )
  )
  

# TODO: clean samples
samples_clean <- samples %>%
  rowwise() %>%
  mutate(
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


# save cleaned data
write_tsv(events_clean, here("data", "03_output", "tango_2_events.tsv"), na = "")
write_tsv(samples_clean, here("data", "03_output", "tango_2_samples.tsv"), na = "")


