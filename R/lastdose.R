
#' @useDynLib lastdose, .registration=TRUE
NULL

#' Calculate last dose amount (LDOS) and time after last dose (TAD)
#'
#' Use [lastdose] to add (or potentially replace) columns to the input
#' data frame; [lastdose_list] and [lastdose_df] returns calculated information
#' as either `list` or `data.frame` format without modifying the input data.
#'
#' @param data data set as data frame; see `details`
#' @param back_calc if `TRUE`, then the time before the first dose
#' is calculated for records prior to the first dosing record when
#' at least one dosing record is found in the data set.  Records before
#' the first dosing record will have negative values.
#' @param fill the value for `TAD` that is used for records when no
#' doses are found for an individual or when `back_calc` is `FALSE`.
#' @param addl_ties what to do when doses scheduled through `ADDL` happen at
#' the same time as observation records; if `obs_first` then the observation
#' is assumed to happen before the dose and the observation is a trough
#' concentration; if `dose_first` then the dose is assumed to be administered
#' and the observation made immediately after (with no advance in time). See
#' details.
#' @param comments a logcial vector with length equal to the number of rows
#' in `data` indicating which records are to be ignored when looking for `TAD`
#' and `LDOS`.  See all the `fill_comments_na` argument.
#' @param ... arguments passed to [lastdose_list]
#' @param include_ldos `logical`; if `FALSE` then the `LDOS` data is not
#' appended to the data set.  Only used for the [lastdose] function.
#' @param fill_comments_na if `TRUE`, then `TAD` and `LDOS` will be filled with
#' `NA` when the comment indicator is `TRUE`; otherwise, commented records
#' are filled in with the time since and amount of the dose on the last
#' non-commented dosing record (the default).
#'
#' @details
#'
#' When calling [lastdose] to modify the data frame, two columns will be
#' added (by default): `TAD` indicating the time after the most-recent dose
#' and `LDOS` indicating the amount of the most recent dose.  This default
#' behavior can be modified with the `include_ldos` argument.
#'
#' When calling [lastdose_list] or [lastdose_df], the respective items are
#' accessible with `tad` and `ldos` (note the lower case form here to
#' distinguish from the columns that might be added to the data frame).
#'
#' **Handling of commented records**: Dosing records that have been "commented"
#' (as indicated with the `comments` argument) will never be considered as
#' actual doses when determining `TAD` and `LDOS`.  But commented records (doses
#' and non-doses) will be assigned `TAD` and `LDOS` according to the last
#' non-commented dosing record by default.  The behavior can be modified by
#' setting through the `fill_comments_na` argument.
#'
#' **Additional notes**:
#'
#' - All functions require an input data set as a data frame
#' - The data set should be formatted according to `NMTRAN` type
#'   conventions
#' - Required columns
#'   - `ID` or `id`: subject identifier
#'   - `TIME` or `time`: data set time
#'   - `AMT` or `amt`: dose amount for dosing records
#'   - `EVID` or `evid`: event ID; records with `EVID` or 1 or 4
#'     are considered dosing records
#' - Optional columns
#'   - `ADDL` or `addl`: additional doses to administer
#'   - `II` or `ii`: dosing interval
#' - An error is generated if required columns are not found; no error
#'   or warning if optional columns are not found
#' - All required and optional columns are required to be numeric
#' - Missing values are not allowed in: `ID`,`EVID`,`ADDL`,`II`
#' - When missing values are found in `TIME`, both `TAD` and `LDOS` are set to
#'   missing
#' - An error is generated for missing `AMT` in dosing records (evid 1 or 4)
#' - No error is generated for missing `AMT` in non-dosing records
#'
#' An example illustrating the `addl_ties` argument: when there is `Q24h`
#' dosing and both an an additional dose and an observation happen at 24 hours,
#' `obs_first` will set the observation`TAD` to 24 and `dose_first` will set
#' the observation `TAD` to 0.
#'
#' @examples
#' file <- system.file("csv/data1.csv", package="lastdose")
#'
#' require("Rcpp")
#'
#' data <- read.csv(file)
#'
#' a <- lastdose(data)
#'
#' b <- lastdose_df(data)
#'
#' c <- lastdose_list(data)
#'
#'
#' @export
lastdose <- function(data,..., include_ldos = TRUE) {
  ans <- lastdose_list(data,...)
  data[["TAD"]] <- ans[["tad"]]
  if(include_ldos) data[["LDOS"]] <- ans[["ldos"]]
  data
}

#' @rdname lastdose
#' @export
lastdose_list <- function(data, fill = -99, back_calc = TRUE,
                          addl_ties = c("obs_first", "dose_first"),
                          comments = find_comments(data),
                          fill_comments_na = FALSE) {
  if(length(comments)==1) {
    comments <- rep(comments,nrow(data))
  }
  if(!length(comments)==nrow(data)) {
    stop("'comments' must be have length equal to the number of rows in 'data'",call.=FALSE)
  }
  addl_ties <- match.arg(addl_ties)
  sort1 <- addl_ties == "obs_first"
  x <- as.data.frame(data)
  na <- tolower(names(data))
  wid <- match("id", na)
  if(is.na(wid)) {
    stop("column ID is required in the data set.", call.=FALSE)
  }
  col_id <- data[[wid]]
  if(!is.numeric(col_id)) {
    stop("column ID/id is required to be numeric", call.=FALSE)
  }
  wtime <- match("time", na)
  if(is.na(wtime)) {
    stop("column TIME or time is required in the data set.", call.=FALSE)
  }
  col_time <- data[[wtime]]
  if(!is.numeric(col_time)) {
    stop("column TIME/time is required to be numeric", call.=FALSE)
  }
  wamt <- match("amt", na)
  if(is.na(wamt)) {
    stop("column AMT or amt is required in the data set.", call.=FALSE)
  }
  col_amt <- data[[wamt]]
  if(!is.numeric(col_amt)) {
    stop("column AMT/amt is required to be numeric", call.=FALSE)
  }
  wevid <- match("evid",na)
  if(is.na(wevid)) {
    stop("column EVID or evid is required in the data set.", call.=FALSE)
  }
  col_evid <- data[[wevid]]
  if(!is.numeric(col_evid)) {
    stop("column EVID/evid is required to be numeric", call.=FALSE)
  }
  waddl <- match("addl", na)
  if(is.na(waddl)) {
    col_addl <- numeric(0)
    wii <- NULL
  } else {
    col_addl <- data[[waddl]]
  }
  if(!is.numeric(col_addl)) {
    stop("column ADDL/addl is required to be numeric", call.=FALSE)
  }
  wii <- match("ii", na)
  if(is.na(wii)) {
    col_ii <- numeric(0)
    wii <- NULL
  } else {
    col_ii <- data[[wii]]
  }
  if(!is.numeric(col_ii)) {
    stop("column II/ii is required to be numeric", call.=FALSE)
  }
  fill <- as.double(fill)
  if(length(fill)==0) fill <- 0
  ans <- lastdose_impl(
    col_id,
    col_time,
    col_amt,
    col_evid,
    col_addl,
    col_ii,
    fill,
    back_calc,
    sort1,
    comments,
    fill_comments_na
  )
  ans
}

#' @rdname lastdose
#' @export
lastdose_df <- function(data,...) {
  ans <- lastdose_list(data,...)
  data.frame(
    tad = ans[["tad"]], ldos = ans[["ldos"]],
    stringsAsFactors=FALSE,check.names=FALSE,
    fix.empty.names=FALSE, row.names=NULL
  )
}

#' Find commented records
#'
#' This function uses specific criteria for finding comment records that may
#' not match up with your coding conventions.  See details below as well as
#' unit tests.
#'
#' @param x a data frame or character vector
#' @param ... not used
#'
#' @return
#' A logical vector
#'
#' @details
#' For the data frame method, comments are found in a column with name `C`
#' with type `character`.  If a `C` column is found that is not character,
#' a warning is generated and no comments are found.
#'
#' For the character method or when an appropriate `C` column is found when
#' using the data frame method, a position or row is considered a comment
#' when it is either `NA` or when it is equal to `.`.
#'
#' @examples
#' comment <- c(NA, "C", "C", NA, ".", NA, "Comment")
#' dv <- rnorm(length(comment))
#' df <- data.frame(C = comment , DV = dv)
#'
#' find_comments(df)
#'
#'
#' @export
find_comments <- function(x,...) UseMethod("find_comments")
#' @rdname find_comments
#'
#' @export
find_comments.data.frame <- function(x,...) {
  if(!is.character(x[["C"]])) {
    if(exists("C", x)) {
      warning("looking for comment records; found column 'C' but is wasn't character.")
    }
    return(vector(mode="logical", nrow(x)))
  }
  find_comments.character(x[["C"]])
}

#' @rdname find_comments
#' @export
find_comments.character <- function(x,...) {
  !(is.na(x)|x=='.')
}

