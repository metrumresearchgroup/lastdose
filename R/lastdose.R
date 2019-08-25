
#' Calculate last dose amount and time after last dose
#'
#' @param data data set as data frame
#' @param fill time after dose value for records prior to the first dose
#'
#' @useDynLib lastdose, .registration=TRUE
#' @export
lastdose <- function(data, fill = -1) {
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
    data[[wid]],
    data[[wtime]],
    data[[wamt]],
    data[[wevid]],
    addl,
    ii,
    as.numeric(fill)
  )
  data.frame(
    tad = ans$tad, ldos = ans$ldos,
    stringsAsFactors=FALSE,check.names=FALSE,
    fix.empty.names=FALSE, row.names=NULL
  )
}
