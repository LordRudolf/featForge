
#' Aggregate Numeric Data by Periods
#'
#' Aggregates any numeric variable(s) in a dataset over defined time periods and returns summary features
#' computed from provided operation functions. E.g., aggregating and making features from transactional data,
#' previous loan repayment behavior, credit bureau inquiries. Aggregation is performed by a specified grouping
#' identifier (e.g., application, client, or agreement level) and is based on time-periods.
#'
#' When \code{period} is provided as a character string (one of \code{"daily"}, \code{"weekly"}, or \code{"monthly"}),
#' data are grouped into complete calendar periods. For example, if the scrape date falls mid-month, the incomplete last period
#' is excluded. Alternatively, \code{period} may be specified as a numeric vector of length 2 (e.g., \code{c(7, 8)}), in which case
#' the first element defines the cycle length in days and the second element the number of consecutive cycles. In this example,
#' if the scrape date is \code{"2024-12-31"}, the periods span the last 56 days (8 consecutive 7-day cycles), with the first period
#' starting on \code{"2024-11-05"}.
#'
#' @param data A data frame containing the data to be aggregated. The dataset must include at least the columns specified by
#'   \code{id_col}, \code{time_col}, and \code{amount_col} (or any numeric variable to aggregate).
#' @param id_col A character string specifying the column name used to define the aggregation level (e.g., \code{"application_id"},
#'   \code{"client_id"}, or \code{"agreement_id"}).
#' @param time_col A character string indicating the column name that contains the date (or timestamp) when the event occurred.
#'   This column must be of class \code{Date} or \code{POSIXct}.
#' @param group_cols An optional character vector of column names by which to further subdivide the aggregation.
#'   For each unique value in these columns, separate summary features will be generated and appended as new columns.
#' @param ops A named list of functions used to compute summary features on the aggregated period values.
#'   Each function must accept a single numeric vector as input. The names of the list elements are used to label the output columns.
#' @param period Either a character string specifying the time period grouping (\code{"daily"}, \code{"weekly"}, \code{"monthly"}, or \code{"all"})
#'   or a numeric vector of length 2 (e.g., \code{c(7, 8)}) where the first element is the cycle length in days and the second is the
#'   number of consecutive cycles. When set to \code{"all"}, the entire set of observations is aggregated as a single period,
#'   effectively disabling time aggregation.
#' @param observation_window_start_col A character string indicating the column name that contains the observation window start date.
#'   This argument is required when \code{period} is specified as a character string other than \code{"all"}.
#' @param scrape_date_col A character string indicating the column name that contains the scrape date (i.e., the end date for the observation
#'   window). This is required when \code{period} is specified as a character string other than \code{"all"} or as a numeric vector.
#' @param period_agg A function used to aggregate the numeric values within each period. The default is \code{sum}. The argument is ignored if \code{period} is \code{"all"}.
#' @param period_missing_inputs A numeric constant used to replace missing values in periods with no observed data. The default value is \code{0}.
#'
#' @details
#' \code{aggregate_applications} aggregates numeric data either by defined time periods or over the full observation window.
#' Data is first grouped by the identifier specified in \code{id_col} (e.g., at the application, client, or agreement level).
#'
#' \enumerate{
#'   \item When \code{period} is set to \code{"daily"}, \code{"weekly"}, or \code{"monthly"},
#'         transaction dates in \code{time_col} are partitioned into complete calendar periods (incomplete periods are excluded).
#'   \item When \code{period} is set to a numeric vector of length 2 (e.g., \code{c(7, 8)}), consecutive cycles of fixed length are defined.
#'   \item When \code{period} is set to \code{"all"}, time aggregation is disabled. All observations for an identifier (or group)
#'         are aggregated together.
#' }
#'
#' For each period, the numeric values in \code{amount_col} (or any other numeric measure) are aggregated using the function
#' specified by \code{period_agg}. Then, for each unique group (if any \code{group_cols} are provided) and for each application (or
#' other identifier), the summary functions specified in \code{ops} are applied to the vector of aggregated period values.
#' When grouping is used, the resulting summary features are appended as new columns with names constructed in the format:
#' \code{<operation>_<group_column>_<group_value>}. Missing aggregated values in periods with no observations are replaced
#' by \code{period_missing_inputs}.
#'
#' @return A data frame where each row corresponds to a unique identifier (e.g., application, client, or agreement).
#'   The output includes aggregated summary features for each period and, if applicable, additional columns for each group
#'   defined in \code{group_cols}.
#'
#' @examples
#' \dontrun{
#' data(featForge_transactions)
#'
#' # Example 1: Aggregate outgoing transactions (amount < 0) on a monthly basis.
#' aggregate_applications(featForge_transactions[featForge_transactions$amount < 0, ],
#'                        id_col = 'application_id',
#'                        amount_col = 'amount',
#'                        time_col = 'transaction_date',
#'                        ops = list(
#'                          avg_momnthly_outgoing_transactions = mean,
#'                          last_month_transactions_amount = function(x) x[length(x)],
#'                          # In the aggregated numeric vector, the last observation represents the most recent period.
#'                          last_month_transaction_amount_vs_mean = function(x) x[length(x)] / mean(x)
#'                        ),
#'                        period = 'monthly',
#'                        observation_window_start_col = 'obs_start',
#'                        scrape_date_col = 'scrape_date'
#' )
#'
#' # Example 2: Aggregate transactions by category and direction.
#' featForge_transactions$direction <- ifelse(featForge_transactions$amount > 0, 'in', 'out')
#' aggregate_applications(featForge_transactions,
#'                        id_col = 'application_id',
#'                        amount_col = 'amount',
#'                        time_col = 'transaction_date',
#'                        group_cols = c('category', 'direction'),
#'                        ops = list(
#'                          avg_monthly_transactions = mean,
#'                          highest_monthly_transactions_count = max
#'                        ),
#'                        period = 'monthly',
#'                        period_agg = length,
#'                        observation_window_start_col = 'obs_start',
#'                        scrape_date_col = 'scrape_date'
#' )
#'
#' # Example 3: Aggregate using a custom numeric period: 30-day cycles for 3 consecutive cycles (i.e., the last 90 days).
#' aggregate_applications(featForge_transactions,
#'                        id_col = 'application_id',
#'                        amount_col = 'amount',
#'                        time_col = 'transaction_date',
#'                        ops = list(
#'                          avg_30_day_transaction_count_last_90_days = mean
#'                        ),
#'                        period = c(30, 3),
#'                        period_agg = length,
#'                        observation_window_start_col = 'obs_start',
#'                        scrape_date_col = 'scrape_date'
#' )
#'
#' # Example 4: Aggregate transactions without time segmentation.
#' aggregate_applications(featForge_transactions,
#'                        id_col = 'application_id',
#'                        amount_col = 'amount',
#'                        ops = list(
#'                          total_transactions_counted = length,
#'                          total_outgoing_transactions_counted = function(x) sum(x < 0),
#'                          total_incoming_transactions_counted = function(x) sum(x > 0)
#'                        ),
#'                        period = 'all'
#' )
#' }
#'
#' @export
aggregate_applications <- function(data,
                                   id_col,
                                   amount_col,
                                   time_col = NULL,
                                   group_cols = NULL,
                                   ops,
                                   period,
                                   observation_window_start_col = NULL,  # required if period is a string and period != 'all'
                                   scrape_date_col = NULL,               # required if period is a string and period != 'all'
                                   period_agg = sum,
                                   period_missing_inputs = 0) {

  #######################
  ## Input Checks ##

  # Check that 'data' is a data frame.
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  # Validate mandatory columns exist.
  validate_col(id_col, data, "id_col")
  validate_col(amount_col, data, "amount_col")

  # If group_cols is provided, ensure it's a character vector and validate each column.
  if (!is.null(group_cols)) {
    if (!is.character(group_cols)) {
      stop("`group_cols` must be a character vector.")
    }
    sapply(group_cols, function(colname) {
      validate_col(colname, data, "group_cols")
    })
  }

  if(!is.null(time_col)) {
    validate_col(time_col, data, "time_col")

    # Check that the timestamp column is of class Date or POSIXct.
    if (!inherits(data[[time_col]], c("Date", "POSIXct"))) {
      stop(sprintf("The column '%s' must be of class Date or POSIXct.", time_col))
    }

    data[[time_col]] <- as.Date(data[[time_col]]) #ensure that we use dates only instead of Posixct or other formats
  }

  # Ensure the amounts column is numeric.
  if (!is.numeric(data[[amount_col]])) {
    stop(sprintf("The column '%s' must be numeric.", amount_col))
  }

  # Validate that 'ops' is a named list of functions.
  if (!is.list(ops) || is.null(names(ops)) || any(names(ops) == "")) {
    stop("`ops` must be a named list of functions.")
  }

  if (!all(sapply(ops, is.function))) {
    stop("All elements in `ops` must be functions.")
  }

  # Validate that 'period_agg' is a function.
  if (!is.function(period_agg)) {
    stop("`period_agg` must be a function.")
  }

  # Validate the 'period' argument.
  if (is.character(period)) {
    allowed_periods <- c("daily", "weekly", "monthly", "all")
    if (!(period %in% allowed_periods)) {
      stop(sprintf("When provided as a string, `period` must be one of: %s",
                   paste(allowed_periods, collapse = ", ")))
    }
    if(period != 'all') {
      if (is.null(observation_window_start_col) || is.null(scrape_date_col) || is.null(time_col)) {
        stop("For period types other than 'all', the columns 'time_col', 'observation_window_start_col' and 'scrape_date_col' must be provided.")
      }
      validate_col(observation_window_start_col, data, "observation_window_start_col")
      validate_col(scrape_date_col, data, "scrape_date_col")

      if (!inherits(data[[observation_window_start_col]], c("Date", "POSIXct"))) {
        stop(sprintf("`%s` must be of class Date or POSIXct.", observation_window_start_col))
      }
      if (!inherits(data[[scrape_date_col]], c("Date", "POSIXct"))) {
        stop(sprintf("`%s` must be of class Date or POSIXct.", scrape_date_col))
      }

      if (any(data[[time_col]] > data[[scrape_date_col]])) {
        stop("Some transaction dates are later than their corresponding scrape_date. Please check your data for inconsistencies.")
      }

      data[[observation_window_start_col]] <- as.Date(data[[observation_window_start_col]])

      check_scrape <- aggregate(data[[scrape_date_col]],
                                by = list(data[[id_col]]),
                                FUN = function(x) length(unique(x)))
      if (any(check_scrape[[2]] > 1)) {
        stop("For at least one application_id, there is more than one unique scrape_date.")
      }
      if (any(is.na(data[[scrape_date_col]]))) {
        stop("NA values found in `scrape_date_col` but that column does not permit NAs.")
      }

      check_obs <- aggregate(data[[observation_window_start_col]],
                             by = list(data[[id_col]]),
                             FUN = function(x) length(unique(x)))
      if (any(check_obs[[2]] > 1)) {
        stop("For at least one application_id, there is more than one unique observation_window_start date.")
      }
      if (any(is.na(data[[observation_window_start_col]]))) {
        stop("NA values found in `observation_window_start_col` but that column does not permit NAs.")
      }
    }
  } else if (is.numeric(period) && length(period) == 2) {
    if (is.null(scrape_date_col) || is.null(time_col)) {
      stop("For numeric period vector, both, `scrape_date_col` and `time_col`, must be provided.")
    }
    validate_col(scrape_date_col, data, "scrape_date_col")
    if (!inherits(data[[scrape_date_col]], c("Date", "POSIXct"))) {
      stop(sprintf("`%s` must be of class Date or POSIXct.", scrape_date_col))
    }
    period <- as.integer(period)
    # Note: For numeric period input, observation_window_start_col is not used in grouping.

  } else if (is.function(period)) {
    stop('The functionality providing `period` as function has not been implemented yet.')

  } else if (is.vector(period)) {

    stop('The functionality providing `period` as a vector has not been implemented yet.')

    if (length(period) != nrow(data)) {
      stop("If period is provided as a vector, its length must match the number of rows in data.")
    }

  } else {
    stop("`period` must be either a character string (daily, weekly, monthly), a function, a numeric vector of length 2 (cycle_length, num_cycles), or a vector.")
  }

  #######################
  ## Data Preparation ##

  if(is.character(period)) {
    if(period == 'all') {
      do_period_aggregation <- FALSE
    } else {
      do_period_aggregation <- TRUE
    }
  } else {
    do_period_aggregation <- TRUE

    # some very un-elegant solution if user hasn't provided observation_window_start_col
    # basically we can accept any column since observation_window_start_col will not be used in the aggregation function below.
    # the necessary column checks happened before
    if(is.null(observation_window_start_col)) {
      observation_window_start_col <- 1
    }
  }

  if(do_period_aggregation) {
    # Process data per application so that each application uses its own scrape_date and observation_window_start.

    data_list <- lapply(split(data, data[[id_col]]), function(df_app) {
      grouping_result <- compute_period_grouping(df_app[[time_col]],
                                                 unique(df_app[[scrape_date_col]]),
                                                 unique(df_app[[observation_window_start_col]]),
                                                 period)
      df_app[["period_group"]] <- grouping_result$period_group
      attr(df_app, "all_periods") <- grouping_result$all_periods

      return(df_app)
    })
  } else {
    data_list <- split(data, data[[id_col]])
  }



  #######################
  ## Aggregation step ##

  if (!is.null(group_cols)) {

    # Precompute the global set of groups for each grouping column
    global_groups_list <- lapply(group_cols, function(g_col) {
      sort(unique(unlist(lapply(data_list, function(df) df[[g_col]]))))
    })
    names(global_groups_list) <- group_cols

    # For each grouping column, compute the global set of groups.
    aggregated_results <- do.call(rbind, lapply(data_list, function(df_app) {
      app_id <- df_app[[id_col]][1]
      group_results_list <- lapply(group_cols, function(g_col) {
        all_groups <- global_groups_list[[g_col]]
        # For each possible group value, aggregate.
        res_list <- lapply(all_groups, function(g_val) {
          df_grp <- df_app[df_app[[g_col]] == g_val, ]
          if (nrow(df_grp) == 0) {
            res <- setNames(as.list(rep(0, length(ops))), names(ops))
          } else {

            if(!do_period_aggregation) {
              res <- sapply(ops, function(fun) fun(df_grp[[amount_col]]))
            } else {
              # Use the attribute from the full application rather than the subset.
              all_periods <- attr(df_app, "all_periods")
              if (is.null(all_periods) || length(all_periods) == 0) {
                res <- setNames(as.list(rep(NA, length(ops))), names(ops))
              } else {
                agg_df <- aggregate(df_grp[[amount_col]],
                                    by = list(period = df_grp$period_group),
                                    FUN = period_agg)
                agg_df$period <- as.Date(agg_df$period)
                complete_periods <- data.frame(period = all_periods)
                agg_df <- merge(complete_periods, agg_df, by = "period", all.x = TRUE, sort = FALSE)
                # Ensure the aggregated column is numeric.
                agg_df[[2]] <- as.numeric(unlist(agg_df[[2]]))
                agg_df[[2]][is.na(agg_df[[2]])] <- period_missing_inputs
                res <- sapply(ops, function(fun) fun(agg_df[[2]]))
              }
            }
            res <- as.list(res)
          }
          # Rename each result with a prefix: opname_gcol_gval
          names(res) <- paste0(names(res), "_", g_col, "_", g_val)
          return(res)
        })
        # Combine the results for the current grouping column.
        do.call(c, res_list)
      })
      # Combine results across all grouping columns.
      combined_groups <- do.call(c, group_results_list)
      # Create a one-row data frame with the application id and all aggregated features.
      result_row <- c(setNames(list(app_id), id_col), combined_groups)
      as.data.frame(result_row, stringsAsFactors = FALSE)
    }))

  } else {
    aggregated_results <- do.call(rbind, lapply(data_list, function(df_app) {
      if(!do_period_aggregation) {
        op_results <- sapply(ops, function(fun) fun(df_app[[amount_col]]))
      } else {
        all_periods <- attr(df_app, "all_periods")
        if (is.null(all_periods) || length(all_periods) == 0) {
          op_results <- setNames(as.list(rep(NA, length(ops))), names(ops))
        } else {
          agg_df <- aggregate(df_app[[amount_col]],
                              by = list(period = df_app$period_group),
                              FUN = period_agg)
          agg_df$period <- as.Date(agg_df$period)
          complete_periods <- data.frame(period = all_periods)
          agg_df <- merge(complete_periods, agg_df, by = "period", all.x = TRUE, sort = FALSE)
          # Ensure the aggregated column is numeric.
          agg_df[[2]] <- as.numeric(unlist(agg_df[[2]]))
          agg_df[[2]][is.na(agg_df[[2]])] <- period_missing_inputs
          op_results <- sapply(ops, function(fun) fun(agg_df[[2]]))
        }
      }
      op_results <- as.list(op_results)
      result_row <- c(as.list(df_app[1, id_col, drop = FALSE]), op_results)
      as.data.frame(result_row, stringsAsFactors = FALSE)
    }))
  }

  return(aggregated_results)
}



# Helper functions for input validation
validate_col <- function(colname, df, arg_name) {
  if (!is.character(colname) || length(colname) != 1) {
    stop(sprintf("`%s` must be a single string.", arg_name))
  }
  if (!colname %in% names(df)) {
    stop(sprintf("The column '%s' specified in `%s` does not exist in the data.",
                 colname, arg_name))
  }
}

validate_col_vector <- function(col_vec, df, arg_name) {
  if (!is.character(col_vec)) {
    stop(sprintf("`%s` must be a character vector.", arg_name))
  }
  missing_cols <- setdiff(col_vec, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("The following columns specified in `%s` do not exist in the data: %s",
                 arg_name, paste0(missing_cols, collapse = ", ")))
  }
}



#' Compute Period Grouping for Aggregation
#'
#' This function computes a period grouping variable for a set of transaction dates
#' given the scrape date and observation window start for a single application.
#'
#' @param time_vec A vector of transaction dates (Date or POSIXct).
#' @param scrape_date A single date representing the scrape date for the application.
#' @param obs_start A single date representing the observation window start for the application.
#' @param period A character string ("daily", "weekly" (starts from Monday), "monthly"), a function, or a vector.
#'        If a function, it should accept time_vec and return a vector of the same length.
#'
#' @return A list with two components:
#'   \code{period_group}: a vector (of the same length as time_vec) with the period label
#'   for each transaction (or NA if the transaction does not fall in a full period).
#'   \code{all_periods}: a sorted vector of all complete periods available.
#'
compute_period_grouping <- function(time_vec, scrape_date, obs_start, period) {
  # Ensure proper date types
  time_vec    <- as.Date(time_vec)
  scrape_date <- as.Date(scrape_date)

  if(!is.null(obs_start)) {
    obs_start   <- as.Date(obs_start)
  }

  if (is.character(period)) {
    if (period == "daily") {
      daily_start <- obs_start + 1
      daily_end   <- scrape_date - 1
      if (daily_start > daily_end) {
        return(list(period_group = as.Date(rep(NA, length(time_vec))),
                    all_periods  = as.Date(character(0))))
      }
      valid_idx   <- time_vec > obs_start & time_vec < scrape_date
      period_group <- as.Date(rep(NA, length(time_vec)))
      period_group[valid_idx] <- time_vec[valid_idx]
      all_periods <- seq(daily_start, daily_end, by = "day")

    } else if (period == "weekly") {
      # For weekly, we require full weeks starting on Monday.
      # Compute ceiling_date: the Monday preceding (or equal to) scrape_date.
      ceiling_date <- scrape_date - ((as.POSIXlt(scrape_date)$wday + 6) %% 7)

      # Compute floor_date: the first Monday after obs_start.
      obs_wday <- as.POSIXlt(obs_start)$wday
      if (obs_wday == 1) {
        floor_date <- obs_start
      } else {
        floor_date <- obs_start + (7 - ((obs_wday + 6) %% 7))
      }

      if (floor_date > ceiling_date - 1) {
        return(list(period_group = as.Date(rep(NA, length(time_vec))),
                    all_periods  = as.Date(character(0))))
      }

      valid_idx <- time_vec < ceiling_date & time_vec >= floor_date
      period_group <- as.Date(rep(NA, length(time_vec)))
      # Align each valid date to the corresponding Monday.
      period_group[valid_idx] <- time_vec[valid_idx] - ((as.POSIXlt(time_vec[valid_idx])$wday + 6) %% 7)

      all_periods <- seq(floor_date, ceiling_date - 1, by = "week")

    } else if (period == "monthly") {
      # For monthly, we require full months starting on the first day.
      obs_date <- obs_start
      if (format(obs_date, "%d") != "01") {
        obs_date <- seq(as.Date(cut(obs_start, "month")), length = 2, by = "month")[2]
      }
      floor_date <- obs_date
      ceiling_date <- as.Date(format(scrape_date, "%Y-%m-01"))
      if (floor_date > ceiling_date - 1) {
        return(list(period_group = as.Date(rep(NA, length(time_vec))),
                    all_periods  = as.Date(character(0))))
      }
      valid_idx <- time_vec < ceiling_date & time_vec >= floor_date
      period_group <- as.Date(rep(NA, length(time_vec)))
      # Convert valid transaction dates to the first day of their month.
      period_group[valid_idx] <- as.Date(format(time_vec[valid_idx], "%Y-%m-01"))
      all_periods <- seq(floor_date, ceiling_date - 1, by = "month")

    } else {
      stop("Unrecognized period type. Supported types are 'daily', 'weekly', or 'monthly'.")
    }

  } else if (is.numeric(period) && length(period) == 2) {

    cycle_length <- period[1]
    num_cycles <- period[2]
    # Compute start_date so that the periods span the last (cycle_length * num_cycles) days ending at scrape_date.
    start_date <- scrape_date - cycle_length * num_cycles
    # Compute the period start dates.
    period_starts <- start_date + (0:(num_cycles - 1)) * cycle_length
    # For each transaction date, assign the corresponding period start if it falls within [start_date, scrape_date].
    period_group <- rep(NA, length(time_vec))
    valid <- time_vec >= start_date & time_vec <= scrape_date
    if (any(valid)) {
      offsets <- as.numeric(time_vec[valid] - start_date)
      period_idx <- floor(offsets / cycle_length) + 1
      # Ensure that any index above num_cycles is capped at num_cycles.
      period_idx[period_idx > num_cycles] <- num_cycles
      period_group[valid] <- period_starts[period_idx]
    }
    all_periods <- period_starts

  } else if (is.function(period)) {
    period_group <- period(time_vec)
    if (length(period_group) != length(time_vec)) {
      stop("The custom period function must return a vector of the same length as time_vec.")
    }
    all_periods <- sort(unique(period_group))

  } else if (is.vector(period)) {
    if (length(period) != length(time_vec)) {
      stop("If period is provided as a vector, its length must match that of time_vec.")
    }
    period_group <- period
    all_periods <- sort(unique(period))

  } else {
    stop("`period` must be a character string, a function, or a vector.")
  }

  return(list(period_group = period_group, all_periods = all_periods))
}
