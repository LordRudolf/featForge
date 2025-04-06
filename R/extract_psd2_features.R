#' Extract Basic Description Features
#'
#' This function processes a vector of text descriptions (such as transaction descriptions) and computes a set of basic text features.
#' These features include counts of digits, special characters, punctuation, words, characters, unique characters, and letter cases,
#' as well as word length statistics and the Shannon entropy of the text.
#'
#' The extracted features are:
#' \describe{
#'   \item{has_digits}{A binary indicator (0/1) showing whether the description contains any digit.}
#'   \item{n_digits}{The total count of digit characters in the description.}
#'   \item{n_special}{The number of special characters (non-alphanumeric and non-whitespace) present.}
#'   \item{n_punct}{The count of punctuation marks found in the description.}
#'   \item{n_words}{The number of words in the description.}
#'   \item{n_chars}{The total number of characters in the description.}
#'   \item{n_unique_chars}{The count of unique characters in the description.}
#'   \item{n_upper}{The count of uppercase letters in the description.}
#'   \item{n_letters}{The total count of alphabetic characters (both uppercase and lowercase) in the description.}
#'   \item{prop_caps}{The proportion of letters in the description that are uppercase.}
#'   \item{n_whitespace}{The number of whitespace characters (spaces) in the description.}
#'   \item{avg_word_length}{The average word length within the description.}
#'   \item{min_word_length}{The length of the shortest word in the description.}
#'   \item{max_word_length}{The length of the longest word in the description.}
#'   \item{entropy}{The Shannon entropy of the description, indicating its character diversity.}
#' }
#'
#' @param descriptions A character vector of text descriptions to be processed.
#'
#' @return A data frame where each row corresponds to an element in \code{descriptions} and each column represents a computed feature.
#'
#' @details
#' The function uses vectorized string operations (e.g., \code{grepl}, \code{gregexpr}, and \code{nchar}) for efficiency,
#' which makes it suitable for processing large datasets. The resulting numeric features can then be used directly for
#' further statistical analysis or machine learning, or they can be aggregated to higher levels.
#'
#' @examples
#' # Example 1: Extract features from a vector of sample descriptions.
#' descs <- c("KappaCredit#101",
#'            "Transferred funds for service fee 990",
#'            "Mighty remittance code 99816 casino")
#' extract_basic_description_features(descs)
#'
#' # Example 2: Aggregate the maximum word length per application.
#' # Load the sample transactions data.
#' data(featForge_transactions)
#'
#' # Combine the transactions data with extracted basic description features.
#' trans <- cbind(featForge_transactions,
#'                extract_basic_description_features(featForge_transactions$description))
#'
#' # Aggregate the maximum word length on the application level.
#' aggregated <- aggregate_applications(
#'   trans,
#'   id_col = "application_id",
#'   amount_col = "max_word_length",
#'   ops = list(max_description_word_length = max),
#'   period = "all"
#' )
#'
#' # Display the aggregated results.
#' aggregated
#'
#' @export
extract_basic_description_features <- function(descriptions) {
  descriptions <- as.character(descriptions)

  has_digits <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    as.numeric(grepl("[0-9]", desc))
  })

  n_digits <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr("[0-9]", desc, perl = TRUE)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  n_special <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr("[^A-Za-z0-9\\s]", desc, perl = TRUE)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  n_punct <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr("[[:punct:]]", desc)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  # Split descriptions into words for further word-level statistics
  word_list <- lapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    strsplit(desc, "\\s+")[[1]]
  })

  n_words <- sapply(word_list, function(words) {
    if (length(words) == 1 && is.na(words[1])) return(NA)
    length(words)
  })

  n_chars <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    nchar(desc)
  })

  n_unique_chars <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    length(unique(strsplit(desc, "")[[1]]))
  })

  n_upper <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr("[A-Z]", desc)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  n_letters <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr("[A-Za-z]", desc)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  prop_caps <- ifelse(is.na(n_letters) | is.na(n_upper), NA,
                      ifelse(n_letters > 0, n_upper / n_letters, 0))

  n_whitespace <- sapply(descriptions, function(desc) {
    if (is.na(desc)) return(NA)
    x <- gregexpr(" ", desc, fixed = TRUE)[[1]]
    if (x[1] == -1) 0 else length(x)
  })

  avg_word_length <- sapply(word_list, function(words) {
    if (length(words) == 1 && is.na(words[1])) return(NA)
    if (length(words) == 0) return(0)
    mean(nchar(words))
  })

  min_word_length <- sapply(word_list, function(words) {
    if (length(words) == 1 && is.na(words[1])) return(NA)
    if (length(words) == 0) return(0)
    min(nchar(words))
  })

  max_word_length <- sapply(word_list, function(words) {
    if (length(words) == 1 && is.na(words[1])) return(NA)
    if (length(words) == 0) return(0)
    max(nchar(words))
  })

  shannon_entropy <- function(text) {
    if (is.na(text)) return(NA)
    if (nchar(text) == 0) return(0)
    chars <- strsplit(text, "")[[1]]
    freq <- table(chars) / length(chars)
    -sum(freq * log2(freq))
  }
  entropy <- sapply(descriptions, shannon_entropy)

  # Remove names from all vectors before constructing the data frame
  data.frame(
    has_digits = unname(has_digits),
    n_digits = unname(n_digits),
    n_special = unname(n_special),
    n_punct = unname(n_punct),
    n_words = unname(n_words),
    n_chars = unname(n_chars),
    n_unique_chars = unname(n_unique_chars),
    n_upper = unname(n_upper),
    n_letters = unname(n_letters),
    prop_caps = unname(prop_caps),
    n_whitespace = unname(n_whitespace),
    avg_word_length = unname(avg_word_length),
    min_word_length = unname(min_word_length),
    max_word_length = unname(max_word_length),
    entropy = unname(entropy),
    stringsAsFactors = FALSE
  )
}


#' Extract Keyword Features from Text Descriptions
#'
#' This function processes a vector of text descriptions (e.g., transaction descriptions) and extracts binary keyword features.
#' Each unique word (token) that meets or exceeds the specified minimum frequency is turned into a column that indicates its presence (1)
#' or absence (0) in each description. For efficiency, the function can create a sparse matrix using the Matrix package.
#' Additionally, the user can choose to convert the output to a standard data.frame.
#'
#' @param descriptions A character vector of text descriptions.
#' @param min_freq A numeric value specifying the minimum frequency a token must have to be included in the output. Default is 1.
#' @param use_matrix Logical. If \code{TRUE} (the default), a sparse matrix is created using the Matrix package.
#' @param convert_to_df Logical. If \code{TRUE} (the default) and \code{use_matrix} is \code{TRUE}, the resulting sparse matrix is converted
#' to a dense data.frame. This is useful for binding with other data.frames, but may be memory-intensive if many unique tokens exist.
#'
#' @return Either a data.frame or a sparse matrix (if \code{convert_to_df} is \code{FALSE}) with one row per description
#' and one column per keyword. Each cell is binary (1 if the keyword is present, 0 otherwise).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts the input text to lowercase and removes digits.
#'   \item Splits the text on any non-letter characters to extract tokens.
#'   \item Constructs a vocabulary from all tokens that appear at least \code{min_freq} times.
#'   \item Depending on the \code{use_matrix} flag, builds either a sparse matrix or a dense matrix of binary indicators.
#'   \item If \code{use_matrix} is \code{TRUE} and \code{convert_to_df} is also \code{TRUE}, the sparse matrix is converted to a data.frame.
#' }
#'
#' @section Warnings:
#' \itemize{
#'   \item If a very large number of descriptions (e.g., more than 100,000) is provided with \code{use_matrix = FALSE}, a warning is issued because
#'         using a dense matrix may exhaust memory.
#'   \item If \code{use_matrix = TRUE}, \code{convert_to_df = TRUE}, and \code{min_freq} is set to 1 (i.e., no filtering), a warning is issued because converting
#'         a very large sparse matrix to a dense data.frame may result in an enormous object.
#' }
#'
#' @examples
#' \dontrun{
#' # Load the sample transactions data.
#' data(featForge_transactions)
#'
#' # Combine the transactions data with extracted keyword features.
#' trans <- cbind(featForge_transactions,
#'                extract_keyword_features(featForge_transactions$description))
#' head(trans) # we see that there have been added categories named "casino" and "utilities".
#'
#' # Aggregate the number of transactions on the application level
#' # by summing the binary keyword indicators for the 'casino' and
#' # 'utilities' columns, using a 30-day period.
#' aggregate_applications(
#'   trans,
#'   id_col = "application_id",
#'   amount_col = "amount",
#'   group_cols = c("casino", "utilities"),
#'   time_col = "transaction_date",
#'   scrape_date_col = "scrape_date",
#'   ops = list(times_different_transactions_in_last_30_days = sum),
#'   period_agg = length,
#'   period = c(30, 1)
#' )
#' }
#'
#' @export
extract_keyword_features <- function(descriptions, min_freq = 1, use_matrix = TRUE, convert_to_df = TRUE) {
  # Ensure descriptions are character vectors
  descriptions <- as.character(descriptions)
  n <- length(descriptions)

  # Warning: Using dense representation for very large datasets may exhaust memory.
  if (!use_matrix && n > 100000) {
    warning("You have provided a large number of descriptions (", n,
            "). Using a dense representation (use_matrix = FALSE) may result in excessive memory usage.")
  }

  # Warning: Converting a sparse matrix to a dense data.frame with min_freq = 1 may lead to an extremely large output.
  if (use_matrix && convert_to_df && min_freq == 1 && n > 100000) {
    warning("Using a sparse matrix with conversion to data.frame (convert_to_df = TRUE) and min_freq = 1 may result in a huge data.frame if many unique tokens are present.")
  }

  # Clean and tokenize: remove digits, convert to lowercase, and split on non-letter characters.
  tokens_list <- lapply(descriptions, function(text) {
    # Remove digits and convert to lowercase
    text_clean <- tolower(gsub("[0-9]+", "", text))
    # Split on one or more non-letter characters; remove empty tokens
    tokens <- unlist(strsplit(text_clean, "[^a-z]+"))
    tokens[tokens != ""]
  })

  # Build overall vocabulary from all tokens and filter by min_freq
  all_tokens <- unlist(tokens_list)
  word_freq <- table(all_tokens)
  vocabulary <- names(word_freq)[word_freq >= min_freq]

  # Create a mapping from word to column index
  vocab_map <- stats::setNames(seq_along(vocabulary), vocabulary)
  V <- length(vocabulary)

  if (V == 0) {
    warning("No words meet the minimum frequency requirement.")
    if (use_matrix) {
      result <- Matrix::sparseMatrix(i = integer(0), j = integer(0), dims = c(n, 0))
    } else {
      result <- data.frame(matrix(numeric(0), nrow = n))
    }
    if (convert_to_df) {
      return(as.data.frame(result, stringsAsFactors = FALSE))
    } else {
      return(result)
    }
  }

  if (use_matrix) {
    # Check if Matrix package is available
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("The 'Matrix' package is required but not installed. Install it or set use_matrix = FALSE.")
    }
    # Prepare vectors to accumulate row and column indices
    row_indices <- integer(0)
    col_indices <- integer(0)

    for (i in seq_len(n)) {
      # Get unique tokens for this description that are in vocabulary
      unique_tokens <- unique(tokens_list[[i]])
      valid_tokens <- unique_tokens[unique_tokens %in% vocabulary]
      if (length(valid_tokens) > 0) {
        cols <- vocab_map[valid_tokens]
        row_indices <- c(row_indices, rep(i, length(cols)))
        col_indices <- c(col_indices, cols)
      }
    }

    # Create sparse binary matrix with ones indicating presence of a word
    sparse_mat <- Matrix::sparseMatrix(i = row_indices,
                                       j = col_indices,
                                       x = rep(1, length(row_indices)),
                                       dims = c(n, V),
                                       dimnames = list(NULL, vocabulary))
    if (convert_to_df) {
      return(as.data.frame(as.matrix(sparse_mat), stringsAsFactors = FALSE))
    } else {
      return(sparse_mat)
    }
  } else {
    # Create a dense matrix of zeros and then fill in binary indicators.
    dense_mat <- matrix(0, nrow = n, ncol = V)
    colnames(dense_mat) <- vocabulary

    for (i in seq_len(n)) {
      unique_tokens <- unique(tokens_list[[i]])
      valid_tokens <- unique_tokens[unique_tokens %in% vocabulary]
      if (length(valid_tokens) > 0) {
        cols <- vocab_map[valid_tokens]
        dense_mat[i, cols] <- 1
      }
    }
    # Return as data.frame
    return(as.data.frame(dense_mat, stringsAsFactors = FALSE))
  }
}



#' Aggregate PSD2 Keyword Features at the Application Level with Time Window Filtering
#'
#' This function extracts keyword features from a transaction descriptions column using the
#' \code{extract_keyword_features} function and then aggregates these features at the application level
#' using the \code{aggregate_applications} function. In addition, when the aggregation period is provided as a numeric
#' vector (e.g., \code{c(30, 3)}), the function filters out transactions that fall outside the observation window
#' defined as the period between \code{scrape_date - (period[1] * period[2])} and \code{scrape_date}. This prevents spending
#' time processing keywords from transactions that would later be aggregated as zeros.
#'
#' The function supports two modes:
#'
#' \itemize{
#'   \item If \code{amount_col} is not provided (i.e., \code{NULL}), the function aggregates keyword counts
#'         (i.e., the number of transactions in which a keyword appears) for each application.
#'   \item If \code{amount_col} is provided, then for each transaction the keyword indicator is multiplied
#'         by the transaction amount. In this mode, the default aggregation operation is to sum these values
#'         (using \code{ops = list(amount = sum)}), yielding the total amount associated with transactions that mention each keyword.
#' }
#'
#' Additionally, if \code{amount_col} is provided and \code{separate_direction} is \code{TRUE} (the default),
#' a new column named \code{"direction"} is created to separate incoming (\code{"in"}) and outgoing (\code{"out"})
#' transactions based on the sign of the amount. Any additional grouping columns can be provided via \code{group_cols}.
#'
#' @param data A data frame containing transaction records.
#' @param id_col A character string specifying the column name that identifies each application (e.g., \code{"application_id"}).
#' @param description_col A character string specifying the column name that contains the transaction descriptions.
#'   Note that this column may contain \code{NA} values.
#' @param amount_col Optional. A character string specifying the column name that contains transaction amounts.
#'   If provided, the function aggregates a value for each keyword (default \code{ops = list(amount = sum)}).
#'   If omitted (\code{NULL}), the function aggregates counts of keyword occurrence (default \code{ops = list(count = sum)}).
#' @param time_col Optional. A character string specifying the column name that contains the transaction date
#'   (or timestamp). When \code{period} is a numeric vector, this is required to filter the data by observation window.
#' @param observation_window_start_col Optional. A character string indicating the column name with the observation window start date.
#'   If \code{period} is not \code{"all"} and is not numeric, this column is used in \code{aggregate_applications}.
#' @param scrape_date_col Optional. A character string indicating the column name with the scrape date.
#'   If \code{period} is not \code{"all"} and is not numeric, this column is used in \code{aggregate_applications}.
#' @param ops A named list of functions used to compute summary features on the aggregated values.
#'   If \code{amount_col} is provided and \code{ops} is \code{NULL}, the default is \code{list(amount = sum)}.
#'   If \code{amount_col} is \code{NULL} and \code{ops} is \code{NULL}, the default is \code{list(count = sum)}.
#' @param period Either a character string or a numeric vector controlling time aggregation.
#'   The default is \code{"all"}, meaning no time segmentation. If a numeric vector is provided (e.g., \code{c(30, 3)}),
#'   it defines a cycle length in days (first element) and a number of consecutive cycles (second element). In that case,
#'   only transactions with a transaction date between \code{scrape_date - (period[1] * period[2])} and \code{scrape_date}
#'   are considered.
#' @param separate_direction Logical. If \code{TRUE} (the default when \code{amount_col} is provided),
#'   a new column \code{"direction"} is added to automatically separate incoming and outgoing transactions based on the sign
#'   of the amount.
#' @param group_cols Optional. A character vector of additional grouping columns to use during aggregation.
#'   If \code{separate_direction} is \code{TRUE}, the \code{"direction"} grouping is added automatically.
#' @param min_freq Numeric. The minimum frequency a token must have to be included in the keyword extraction.
#'   Default is 1.
#' @param use_matrix Logical. Passed to \code{extract_keyword_features}; if \code{TRUE} (the default) a sparse matrix is used.
#' @param convert_to_df Logical. Passed to \code{extract_keyword_features}; if \code{TRUE} (the default) the sparse matrix
#'   is converted to a data.frame, facilitating binding with other data.
#' @param period_agg A function used to aggregate values within each period (see \code{aggregate_applications}).
#'   Default is \code{sum}.
#' @param period_missing_inputs A numeric value to replace missing aggregated values. Default is \code{0}.
#'
#' @return A data frame with one row per application and aggregated keyword features.
#'
#' @details
#' The function performs the following steps:
#'
#' \enumerate{
#'   \item Basic input checks are performed to ensure the required columns exist.
#'   \item The full list of application IDs is stored from the original data.
#'   \item If \code{amount_col} is provided and \code{separate_direction} is \code{TRUE}, a \code{"direction"} column is added
#'         to label transactions as incoming (\code{"in"}) or outgoing (\code{"out"}) based on the sign of the amount.
#'   \item When \code{period} is provided as a numeric vector, the function computes the observation window as
#'         \code{scrape_date - (period[1] * period[2])} to \code{scrape_date} and filters the dataset to include only transactions
#'         within this window. Transactions for applications with no records in the window will later be assigned zeros.
#'   \item Keyword features are extracted from the \code{description_col} using \code{extract_keyword_features}.
#'         If an \code{amount_col} is provided, the binary indicators are weighted by the transaction amount.
#'   \item The extracted keyword features are combined with the (possibly filtered) original data.
#'   \item For each keyword, the function calls \code{aggregate_applications} to aggregate the feature by application.
#'         The aggregation is performed over time periods defined by \code{period} (if applicable) and, if requested,
#'         further split by direction.
#'   \item Aggregated results for each keyword are merged by application identifier.
#'   \item Finally, the aggregated results are merged with the full list of application IDs so that applications
#'         with no transactions in the observation window appear with zeros.
#' }
#'
#' @examples
#' \dontrun{
#' # Example: Aggregate keyword features for PSD2 transactions.
#'
#' data(featForge_transactions)
#'
#' # In this example, the 'description' field is parsed for keywords.
#' # Since the 'amount' column is provided, each keyword indicator is
#' # weighted by the transaction amount, and transactions are
#' # automatically split into incoming and outgoing via the 'direction' column.
#' # Additionally, the period is specified as c(30, 1), meaning only
#' # transactions occurring within the last 30 days.
#' # (scrape_date - 30 to scrape_date) are considered.
#' result <- aggregate_psd2_keyword_features(
#'   data = featForge_transactions,
#'   id_col = "application_id",
#'   description_col = "description",
#'   amount_col = "amount",
#'   time_col = "transaction_date",
#'   scrape_date_col = "scrape_date",
#'   observation_window_start_col = "obs_start",
#'   period = c(30, 1),
#'   ops = list(amount = sum),
#'   min_freq = 1,
#'   use_matrix = TRUE,
#'   convert_to_df = TRUE
#' )
#'
#' # The resulting data frame 'result' contains one
#' # row per application with aggregated keyword features.
#' # For example, if keywords "casino" and "utilities" were detected,
#' # aggregated columns might be named:
#' # "casino_amount_direction_in",
#' # "casino_amount_direction_out",
#' # "utilities_amount_direction_in", etc.
#' result
#' }
#'
#' @export
aggregate_psd2_keyword_features <- function(data,
                                            id_col,
                                            description_col,
                                            amount_col = NULL,
                                            time_col = NULL,
                                            observation_window_start_col = NULL,
                                            scrape_date_col = NULL,
                                            ops = NULL,
                                            period = "all",
                                            separate_direction = if(!is.null(amount_col)) TRUE else FALSE,
                                            group_cols = NULL,
                                            min_freq = 1,
                                            use_matrix = TRUE,
                                            convert_to_df = TRUE,
                                            period_agg = sum,
                                            period_missing_inputs = 0) {
  # Store the full list of application IDs before any filtering.
  orig_ids <- unique(data[[id_col]])

  # Basic input checks.
  if (!(id_col %in% names(data))) stop("id_col not found in data.")
  if (!(description_col %in% names(data))) stop("description_col not found in data.")
  if (!is.null(amount_col)) {
    if (!(amount_col %in% names(data))) stop("amount_col not found in data.")
    if (!is.numeric(data[[amount_col]])) stop("amount_col must be numeric.")
  }

  # Set default ops based on the presence of an amount column.
  if (is.null(amount_col)) {
    if (is.null(ops)) {
      ops <- list(count = sum)
    }
  } else {
    if (is.null(ops)) {
      ops <- list(amount = sum)
    }
  }

  # If amount_col is provided and separate_direction is TRUE, create a 'direction' column.
  if (!is.null(amount_col) && separate_direction) {
    data$direction <- ifelse(data[[amount_col]] >= 0, "in", "out")
  }

  # If period is provided as a numeric vector, filter the data to include only transactions
  # within the observation window: [scrape_date - (period[1] * period[2]), scrape_date].
  if (is.numeric(period) && length(period) == 2) {
    if (is.null(time_col) || is.null(scrape_date_col)) {
      stop("For numeric period vector, time_col and scrape_date_col must be provided.")
    }
    data[[time_col]] <- as.Date(data[[time_col]])
    data[[scrape_date_col]] <- as.Date(data[[scrape_date_col]])
    window_length <- period[1] * period[2]
    data <- data[data[[time_col]] >= (data[[scrape_date_col]] - window_length) &
                   data[[time_col]] <= data[[scrape_date_col]], ]
  }

  # Extract keyword features from the description column.
  keyword_df <- extract_keyword_features(data[[description_col]],
                                         min_freq = min_freq,
                                         use_matrix = use_matrix,
                                         convert_to_df = convert_to_df)

  # If an amount column is provided, weight the keyword indicators by the transaction amount.
  if (!is.null(amount_col)) {
    for (col in names(keyword_df)) {
      keyword_df[[col]] <- keyword_df[[col]] * data[[amount_col]]
    }
  }

  # Combine the original (possibly filtered) data with the extracted keyword features.
  new_data <- cbind(data, keyword_df)

  # Compute aggregated results for each keyword using lapply instead of a for loop.
  keyword_names <- names(keyword_df)
  agg_results_list <- lapply(keyword_names, function(kw) {
    # Set grouping: if separate_direction is TRUE and amount_col is provided, include "direction".
    agg_group_cols <- group_cols
    if (!is.null(amount_col) && separate_direction) {
      if (is.null(agg_group_cols)) {
        agg_group_cols <- "direction"
      } else {
        agg_group_cols <- unique(c("direction", agg_group_cols))
      }
    }

    temp <- aggregate_applications(
      data = new_data,
      id_col = id_col,
      amount_col = kw,
      time_col = time_col,
      group_cols = agg_group_cols,
      ops = ops,
      period = period,
      observation_window_start_col = observation_window_start_col,
      scrape_date_col = scrape_date_col,
      period_agg = period_agg,
      period_missing_inputs = period_missing_inputs
    )

    # Rename the aggregated columns to include the keyword as a prefix.
    agg_names <- setdiff(names(temp), id_col)
    names(temp)[names(temp) != id_col] <- paste(kw, agg_names, sep = "_")
    temp
  })

  # Merge all aggregated keyword results by the application id.
  final_agg <- Reduce(function(x, y) merge(x, y, by = id_col, all = TRUE), agg_results_list)

  # Merge with the full list of original application IDs, filling missing rows with zeros.
  full_ids_df <- stats::setNames(data.frame(orig_ids, stringsAsFactors = FALSE), id_col)
  final_agg_full <- merge(full_ids_df, final_agg, by = id_col, all.x = TRUE)

  # Replace NA aggregated values with 0.
  agg_cols <- setdiff(names(final_agg_full), id_col)
  for (col in agg_cols) {
    final_agg_full[[col]][is.na(final_agg_full[[col]])] <- 0
  }

  return(final_agg_full)
}
