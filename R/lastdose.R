
#' Calculate last dose amount and time after last dose
#'
#' Use [lastdose] to columns to the input data frame; [lastdose_list]
#' and [lastdose_df] returns calculated information as either
#' `list` or `data.frame` format without modifying the input data.
#'
#' @param data data set as data frame; see `details`
#' @param fill time after dose value for records prior to the first dose
#' @param ... arguments passed to [lastdose_list]
#'
#' @details
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
#' - All required and optional columns will be coerced with [as.double]
#'
#' @useDynLib lastdose, .registration=TRUE
#' @export
lastdose <- function(data,...) {
  ans <- lastdose_list(data,...)
  data[["TAD"]] <- ans[["tad"]]
  data[["LDOS"]] <- ans[["ldos"]]
  data
}

#' @rdname lastdose
#' @export
lastdose_list <- function(data, fill = -1) {
  x <- as.data.frame(data)
  na <- tolower(names(data))
  wid <- match("id", na)
  if(is.na(wid)) {
    stop("column ID is required in the data set.", call.=FALSE)
  }
  wtime <- match("time", na)
  if(is.na(wtime)) {
    stop("column TIME or time is required in the data set.", call.=FALSE)
  }
  wamt <- match("amt", na)
  if(is.na(wamt)) {
    stop("column AMT or amt is required in the data set.", call.=FALSE)
  }
  wevid <- match("evid",na)
  if(is.na(wevid)) {
    stop("column EVID or evid is required in the data set.", call.=FALSE)
  }
  waddl <- match("addl", na)
  if(is.na(waddl)) {
    addl <- numeric(0)
  } else {
    addl <- data[[waddl]]
  }
  wii <- match("ii", na)
  if(is.na(wii)) {
    ii <- numeric(0)
  } else {
    ii <- data[[wii]]
  }
  ans <- lastdose_impl(
    as.double(data[[wid]]),
    as.double(data[[wtime]]),
    as.double(data[[wamt]]),
    as.double(data[[wevid]]),
    as.double(addl),
    as.double(ii),
    as.numeric(fill)
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

