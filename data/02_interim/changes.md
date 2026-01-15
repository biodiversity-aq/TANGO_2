 | file name
---|---
input file | `01_raw/TANGO_2_DATA.xlsx`
output file | `02_interim/TANGO_2_DATA_cleaned.xlsx`


# Changes

## Events sheet

### add recordedByID

### add eventTime

- for events that do not span more than 1 day
- `=TEXT(E2*86400/86400,"hh:mm:ss") & "-03:00/" & TEXT(E2*86400/86400,"hh:mm:ss") & "-03:00"`
- paste as value for the column
- leave eventTime = Time start (UTC-3) if Time end is empty

### delete nonsense Duration value

- arise from empty time end
- miscalculation for events that span multiple days

### add eventDate

```
=CONCAT(
  TEXT(C91,"yyyy-mm-dd"),"T",TEXT(E91,"hh:mm"),"-03:00/",
  TEXT(D91,"yyyy-mm-dd"),"T",TEXT(F91,"hh:mm"),"-03:00"
)

```
for events that span multiple days

### rename fields

old name | new name
---|---
Station Name | locality
Dept Min (m) | minimumDepthInMeters
Dept Max (m) | maximumDepthInMeters
Alt Max (m) | maximumElevationInMeters
Remarks | eventRemarks

## Samples sheet

### add identifiedByID1,2,3,4

### add higherGeographyID

using SCAR Composite Gazetteer based on station name/locality


### rename fields

old name | new name
---|---
ScientificName | verbatimScientificName


### correct typo 

verbatimScientificName | verbatimIdentification
---|---
Cyamomactra laminifera | Cyamiomactra laminifera
Hirudinae | Hirudinea

### remove all trailing space in verbatimIdentification

### delete empty columns

- institutionCode