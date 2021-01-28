# lastdose (development version)

- Fix bug where `II` column was not properly detected resulting in incorrect 
  `TAD` calculation when doses were also scheduled via `ADDL` #21
  
# lastdose 0.3.0

- Add `time_col` and `id_col` arguments to let user name columns containing 
  time and user id information #14
- ID column is allowed to be character; data will be converted to numeric if 
  it is character #14
- Time column is allowed to be `POSIXct`; data will be converted to numeric 
  according to value of `time_units` argument #9

# lastdose 0.2.1
- Fix bug where behavior is undefined when ADDL or II columns are omitted
  (#11)

# lastdose 0.2.0
- Commented records are not considered when looking for doses in the data set, 
  but `TAD` and `LDOS` are filled like any other observation record (relative
  to the last non-commented dose) (#9)

# lastdose 0.1.0

- Update git origin

