
# lastdose

## User Stories

1. I am assembling an analysis data set and I wish to include a column 
   that indicates the amount of time since the last dose was administered 
   (`TAD`).
1. I am assembling an analysis data set and I wish to include a column 
   that indicates the amount of the last dose that was administered (`LDOS`). 
1. I would like to calculate `TAD` or `LDOS` and get that information back 
   in a format of my choice.
1. I would like to be able to choose how `TAD` is filled in for records 
   prior to the first dosing record.
   
## Summary of requirements

1. The user will pass in NMTRAN-formatted analysis data as a data frame. 
1. The analysis data frame will have the following required columns
    - `ID` or `id` - the user id number
    - `TIME` or `time` - the time since first observation 
    - `AMT` or `amt` - the dose amount
    - `EVID` or `evid` - the record event id; `evid` equal to 1 or 4
      will be considered dosing records
1. The analysis data frame will have the following optional columns
    - `ADDL` or `addl` - additional doses to administer
    - `II` or `ii` - the dosing interval
1. All required and optional columns will be coerced using `as.double`.
1. By default, doses prior to the first dosing record will be calculated as 
   time before the first dose and be expressed as negative numbers.
1. The user may choose to fill in `TAD` for records prior to the first dose
   with a single user-specified fill value.
1. The single use-specified fill value will be use to fill in all values for 
   `TAD` when no dosing records are found for a particular individual.
1. The `lastdose()` function will append columns called `TAD` and `LDOS` to 
   the input data indicating the time after dose and last dose, respectively.
1. The `lastdose_list()` function will do the same calculations as `lastdose()`, 
   however a list with elements `tad` and `ldos` will be returned.
1. The `lastdose_df()` function will return a data frame with columns `tad`
   and `ldos` with the same definitions as `lastdose_list()`.
1. The user can choose how to handle `TAD` when additional doses (via `ADDL`
   happen at the same time as another record in the data set.  By default, 
   records at the same time are sorted ascending by dose amount so that 
   observation records happen prior to doses.  This makes the observation a 
   trough concentration. Alternatively, records can be sorted descending 
   by dose amount so that doses happen first.  This makes the observation 
   a peak. The sorting that is done has no bearing on records explicitly listed
   in the input data. 





