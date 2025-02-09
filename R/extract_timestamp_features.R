#' Extract Timestamp Features
#'
#' This function extracts various features from application timestamps and, if provided, client dates of birth.
#' It supports both \code{POSIXct} and \code{Date} objects for \code{timestamps}. If the timestamps are given as \code{Date} objects,
#' note that features requiring intra-day granularity (e.g., \code{timestamp_hour}) will not be created, and some cyclic features may be less precise.
#'
#' The function returns a data frame containing the following variables:
#' \describe{
#'   \item{timestamp_month}{Numeric. Month extracted from the timestamp (1 to 12).}
#'   \item{timestamp_month_sine}{Numeric. Sine transformation of the month (using period = 12).}
#'   \item{timestamp_month_cosine}{Numeric. Cosine transformation of the month (using period = 12).}
#'   \item{timestamp_day_of_month}{Numeric. Day of the month extracted from the timestamp (1 to 31).}
#'   \item{timestamp_day_of_month_sine}{Numeric. Sine transformation of the day of the month (using period = 31).}
#'   \item{timestamp_day_of_month_cosine}{Numeric. Cosine transformation of the day of the month (using period = 31).}
#'   \item{timestamp_week_of_year}{Numeric. ISO week number extracted from the timestamp (typically 1 to 52, but may be 53 in some years).}
#'   \item{timestamp_week_of_year_sine}{Numeric. Sine transformation of the week of the year (using period = 52).}
#'   \item{timestamp_week_of_year_cosine}{Numeric. Cosine transformation of the week of the year (using period = 52).}
#'   \item{timestamp_day_of_week}{Numeric. Day of the week extracted from the timestamp (1 for Monday through 7 for Sunday).}
#'   \item{timestamp_day_of_week_sine}{Numeric. Sine transformation of the day of the week (using period = 7).}
#'   \item{timestamp_day_of_week_cosine}{Numeric. Cosine transformation of the day of the week (using period = 7).}
#'   \item{timestamp_hour}{Numeric. Hour of the day (0 to 23). This is only available if \code{timestamps} are of class \code{POSIXct}.}
#'   \item{timestamp_hour_sine}{Numeric. Sine transformation of the hour (using period = 24).}
#'   \item{timestamp_hour_cosine}{Numeric. Cosine transformation of the hour (using period = 24).}
#'   \item{client_age_at_application}{Numeric. Client's age at the time of application, calculated in years (real number).}
#'   \item{days_to_birthday}{Numeric. Number of days until the client's next birthday.}
#'   \item{days_to_birthday_cosine}{Numeric. Cosine transformation of the days to birthday (using period = 365).}
#' }
#'
#' @param timestamps A vector of timestamps, either as \code{POSIXct} or \code{Date} objects.
#' @param date_of_birth An optional vector of client dates of birth. If provided, it must have the same length as \code{timestamps},
#'   enabling computation of age and birthday-related features.
#' @param error_on_invalid Logical flag specifying whether to throw an error (\code{TRUE}) or a warning (\code{FALSE}, default)
#'   when missing or invalid timestamp values are detected.
#'
#' @details The function first validates the inputs and then extracts the following features:
#' \itemize{
#'   \item Extracts date-based features such as year, month, day of month, ISO week of the year, and day of the week.
#'   \item If timestamps are of class \code{POSIXct}, it also extracts the hour of the day.
#'   \item Applies cyclic transformations (using sine and cosine functions) to the month, day of month, week of year, day of week,
#'         and hour variables so that their cyclical nature is maintained in machine learning models.
#'   \item If \code{date_of_birth} is provided, computes the client's age at the time of application and the number of days
#'         until their next birthday, along with a cosine transformation for the latter.
#' }
#'
#' @examples
#' \dontrun{
#' # Load sample data
#' data(featForge_sample_data)
#'
#' # Generate features and combine with the original dataset
#' result <- cbind(
#'   data.frame(
#'     application_created_at = featForge_sample_data$application_created_at,
#'     client_date_of_birth = featForge_sample_data$date_of_birth
#'   ),
#'   extract_timestamp_features(
#'     featForge_sample_data$application_created_at,
#'     featForge_sample_data$date_of_birth
#'   )
#' )
#'
#' head(result)
#' }
#'
#' @return A data frame with the extracted timestamp features and birthday-related features (if \code{date_of_birth} is provided).
#' @export

extract_timestamp_features <- function(timestamps,
                                       date_of_birth = NULL,
                                       error_on_invalid = FALSE) {

  #######################
  ## Basic input checks

  if (!inherits(timestamps, c("POSIXct", "Date"))) {
    stop("'timestamps' must be either POSIXct or Date.")
  }

  is_date <- inherits(timestamps, "Date")
  if (is_date) {
    warning("Timestamps are provided as 'Date' objects. Some features requiring time-of-day granularity will not be created.")
  }

  invalid_indices <- which(is.na(timestamps))

  if (length(invalid_indices) > 0) {
    msg <- "There are missing values in 'timestamps'."
    if (error_on_invalid) {
      stop(msg)
    } else {
      warning(msg)
    }
  }

  if (!is.null(date_of_birth)) {
    if (!inherits(date_of_birth, "Date")) {
      stop("'date_of_birth' must be of class 'Date' if provided.")
    }
    if (length(date_of_birth) != length(timestamps)) {
      stop("Length of 'date_of_birth' must match length of 'timestamps'.")
    }
    client_dob_provided <- TRUE
  } else {
    client_dob_provided <- FALSE
  }

  #######################
  ## Helper functions

  extract_period <- function(x, period, alias) {

    ## TO DO: adjust sine and cosine for different month lengths considering it can range from 28 to 31

    temp <- data.frame(
      timestamp_ = x
    )
    colnames(temp) <- paste0('timestamp_', alias)

    temp[[paste0('timestamp_', alias, '_sine')]] <- cyclical_sin(temp[[1]], period)
    temp[[paste0('timestamp_', alias, '_cosine')]] <- cyclical_cos(temp[[1]], period)

    return(temp)
  }

  #######################
  ## General features

  res <- cbind(
    extract_period(as.integer(format(timestamps, '%m')), 12, 'month'),
    extract_period(as.integer(format(timestamps, '%d')), 31, 'day_of_month'),
    extract_period(as.integer(format(timestamps, '%V')), 53, 'week_of_year'),
    extract_period(as.integer(format(timestamps, '%u')), 7, 'day_of_week')
  )

  if(!is_date) {
    res <- cbind(
      res,
      extract_period(as.integer(format(timestamps, '%H')), 24, 'hour')
    )
  }

  #######################
  ## Date of birth related features

  if(client_dob_provided) {
    timestamps_date <- as.Date(timestamps)

    res$client_age_at_application <- as.numeric(difftime(timestamps_date, date_of_birth, units = "days")) / 365.25

    dob_mm_dd <- format(date_of_birth, "%m-%d")
    ts_year <- as.integer(format(timestamps_date, "%Y"))

    this_year_bday <- as.Date(paste0(ts_year, "-", dob_mm_dd))

    # If today's timestamp is already past the birthday, add 1 year
    is_birthday_passed <- (this_year_bday < timestamps_date) & !is.na(this_year_bday)
    next_birthday <- this_year_bday
    next_birthday[is_birthday_passed] <- as.Date(format(this_year_bday[is_birthday_passed] + 365, "%Y-%m-%d"))


    res$days_to_birthday <- as.numeric(difftime(next_birthday, timestamps_date, units = "days"))
    res$days_to_birthday_cosine <-cyclical_cos(res$days_to_birthday, 365)
  }


  return(res)
}
