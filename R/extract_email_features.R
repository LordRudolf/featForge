#' Extract Email Features for Credit Scoring
#'
#' This function processes a vector of email addresses to extract a comprehensive set of features that can be useful for credit scoring.
#' In addition to parsing the email into its constituent parts (such as the username and domain), the function computes various character-level
#' statistics (e.g., counts of digits, dots, uppercase letters) and string distance metrics between the email username and client name information.
#' If provided, it also checks for the presence of date-of-birth components in the email username (in several flexible formats).
#'
#' @param emails A character vector of email addresses. Invalid email addresses are either replaced with \code{NA} (with a warning) or cause an error,
#'   depending on the value of \code{error_on_invalid}.
#' @param error_on_invalid Logical. If \code{TRUE}, the function will throw an error when encountering an invalid email address.
#'   If \code{FALSE} (the default), invalid emails are replaced with \code{NA} and a warning is issued.
#' @param client_name Optional. A character vector of client first names. When provided, its length must equal that of \code{emails}.
#' @param client_surname Optional. A character vector of client surnames. When provided, its length must equal that of \code{emails}.
#' @param date_of_birth Optional. A \code{Date} vector containing the client's dates of birth. When provided, its length must equal that of \code{emails}.
#' @param error_on_invalid Logical. If \code{TRUE}, the function will throw an error when encountering an invalid email address.
#'   If \code{FALSE} (the default), invalid emails are replaced with \code{NA} and a warning is issued.
#'
#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{\code{email_domain}}{The domain part of the email address (i.e., the substring after the '@').}
#'     \item{\code{email_major_domain}}{The major domain extracted as the substring after the last dot in the domain (e.g., "com" in "gmail.com").}
#'     \item{\code{email_n_chars}}{The number of characters in the email username (i.e., the part before the '@').}
#'     \item{\code{email_n_digits}}{The number of digits found in the email username.}
#'     \item{\code{email_n_dots}}{The number of dot ('.') characters in the email username.}
#'     \item{\code{email_n_caps}}{The number of uppercase letters in the email username.}
#'     \item{\code{email_total_letters}}{The total count of alphabetic characters (both uppercase and lowercase) in the email username.}
#'     \item{\code{email_prop_digits}}{The proportion of digits in the email username (calculated as \code{email_n_digits/email_n_chars}).}
#'     \item{\code{email_max_consecutive_digits}}{The maximum length of any sequence of consecutive digits in the email username.}
#'     \item{\code{email_name_in_email}}{Logical. \code{TRUE} if the provided client name is found within the email username (case-insensitive), \code{FALSE} otherwise.}
#'     \item{\code{email_name_in_email_dist_lv}}{The Levenshtein distance between the client name and the email username (if the \code{stringdist} package is available; otherwise \code{NA}).}
#'     \item{\code{email_name_in_email_dist_lcs}}{The Longest Common Subsequence distance between the client name and the email username (if computed).}
#'     \item{\code{email_name_in_email_dist_cosine}}{The cosine distance between the client name and the email username (if computed).}
#'     \item{\code{email_name_in_email_dist_jaccard}}{The Jaccard distance between the client name and the email username (if computed).}
#'     \item{\code{email_name_in_email_dist_jw}}{The Jaro-Winkler distance between the client name and the email username (if computed).}
#'     \item{\code{email_name_in_email_dist_soundex}}{The Soundex distance between the client name and the email username (if computed).}
#'     \item{\code{email_surname_in_email}}{Logical. \code{TRUE} if the provided client surname is found within the email username (case-insensitive), \code{FALSE} otherwise.}
#'     \item{\code{email_surname_in_email_dist_lv}}{The Levenshtein distance between the client surname and the email username (if computed).}
#'     \item{\code{email_surname_in_email_dist_lcs}}{The Longest Common Subsequence distance between the client surname and the email username (if computed).}
#'     \item{\code{email_surname_in_email_dist_cosine}}{The cosine distance between the client surname and the email username (if computed).}
#'     \item{\code{email_surname_in_email_dist_jaccard}}{The Jaccard distance between the client surname and the email username (if computed).}
#'     \item{\code{email_surname_in_email_dist_jw}}{The Jaro-Winkler distance between the client surname and the email username (if computed).}
#'     \item{\code{email_surname_in_email_dist_soundex}}{The Soundex distance between the client surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_lv}}{The Levenshtein distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_lcs}}{The Longest Common Subsequence distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_cosine}}{The cosine distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_jaccard}}{The Jaccard distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_jw}}{The Jaro-Winkler distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_fullname_in_email_dist_soundex}}{The Soundex distance between the concatenated client name and surname and the email username (if computed).}
#'     \item{\code{email_has_full_year_of_birth}}{Logical. \code{TRUE} if the full 4-digit year (e.g., "1986") of the client's date of birth is present in the email username.}
#'     \item{\code{email_has_last_two_digits_of_birth}}{Logical. \code{TRUE} if the last two digits of the client's birth year are present in the email username.}
#'     \item{\code{email_has_full_dob_in_username}}{Logical. \code{TRUE} if the full date of birth (in one of the following formats: YYYYMMDD, YYYY.MM.DD, YYYY_MM_DD, or YYYY-MM-DD) is present in the email username.}
#'     \item{\code{email_has_other_4digit_year}}{Logical. \code{TRUE} if a different 4-digit year (between 1920 and 2020) is found in the email username that does not match the client's own birth year.}
#'   }
#'
#' @details
#' The function is designed to support feature engineering for credit-scoring datasets. It not only extracts parts of an email address
#' (such as the username and domain) but also computes detailed characteristics from the username, which may include embedded client information.
#'
#' When client name information is provided, the function computes various string distance metrics (using the \code{stringdist} package) between
#' the client name (and surname) and the email username. If \code{stringdist} is not installed, the function will issue a warning and assign \code{NA}
#' to the distance-based features.
#'
#' @examples
#'  # Load sample data included in the package
#'  data("featForge_sample_data")
#'
#' # Extract features from the sample emails
#'  features <- extract_email_features(
#'    emails = featForge_sample_data$email,
#'    client_name = featForge_sample_data$client_name,
#'    client_surname = featForge_sample_data$client_surname,
#'    date_of_birth = featForge_sample_data$date_of_birth
#'  )
#'
#' # Display the first few rows of the resulting feature set
#' head(features)
#'
#' @seealso \code{\link[stringdist]{stringdist}} for the calculation of string distances.
#'
#' @export
extract_email_features <- function(emails,
                                   client_name = NULL,
                                   client_surname = NULL,
                                   date_of_birth = NULL,
                                   error_on_invalid = FALSE) {

  #######################
  ## Basic input checks

  if (!is.character(emails)) {
    stop("'emails' must be a character vector.")
  }

  if (!is.null(client_name)) {
    if (!is.character(client_name)) {
      stop("'client_name' must be a character vector if provided.")
    }
    if (length(client_name) != length(emails)) {
      stop("Length of 'client_name' must match length of 'emails'.")
    }
    client_name_provided <- TRUE
  } else{
    client_name_provided <- FALSE
  }

  if (!is.null(client_surname)) {
    if (!is.character(client_surname)) {
      stop("'client_surname' must be a character vector if provided.")
    }
    if (length(client_surname) != length(emails)) {
      stop("Length of 'client_surname' must match length of 'emails'.")
    }
    client_surname_provided <- TRUE
  } else {
    client_surname_provided <- FALSE
  }

  if (!is.null(date_of_birth)) {
    if (!inherits(date_of_birth, "Date")) {
       stop("'date_of_birth' must be of class 'Date' if provided.")
     }
    if (length(date_of_birth) != length(emails)) {
      stop("Length of 'date_of_birth' must match length of 'emails'.")
    }
    client_dob_provided <- TRUE
  } else {
    client_dob_provided <- FALSE
  }

  if(client_name_provided && client_surname_provided) {
    full_name <- make.names(paste0(tolower(client_name), '.', tolower(client_surname)))
  }

  # Define a simple regex pattern for valid emails.
  email_pattern <- "^[^@]+@[^@]+\\.[^@]+$"


  # Apply the regex to non-missing emails.
  non_missing <- !is.na(emails)
  is_valid_email <- rep(FALSE, length(emails))
  is_valid_email[non_missing] <- grepl(email_pattern, emails[non_missing])

  #Handle invalid emails based on 'error_on_invalid' setting.
  invalid_indices <- which(non_missing & !is_valid_email)


  if (length(invalid_indices) > 0) {
    if (error_on_invalid) {
      stop(
        "Invalid email addresses found. Set `error_on_invalid = FALSE` ",
        "to replace invalid values with NA instead."
      )
    } else {
      # Replace invalid emails with NA and warn.
      emails[invalid_indices] <- NA
      warning("Some invalid email addresses were replaced with NA. ",
              "Set `error_on_invalid = TRUE` to throw an error instead.")
    }
  }

  #######################
  ## Helper functions

  safeGregCount <- function(x, pattern) {
    # If x is NA, return NA
    if (is.na(x)) return(NA_integer_)
    matches <- gregexpr(pattern, x, perl = TRUE)[[1]]
    if (matches[1] == -1) {
      return(0L)
    } else {
      return(length(matches))
    }
  }

  safeTotalLetters <- function(x) {
    if (is.na(x)) return(NA_integer_)
    matches <- gregexpr("[A-Za-z]", x, perl = TRUE)[[1]]
    if (matches[1] == -1) {
      0L
    } else {
      length(matches)
    }
  }

  calculate_string_distance <- function(username, other_name, prefix) {
    temp <- data.frame(
      #osa = stringdist::stringdist(tolower(username), tolower(other_name), method = "osa"), #high correlation with Levenstein distance
      #dl = stringdist::stringdist(tolower(username), tolower(other_name), method = "dl"), #high correlation with Levenstein distance
      #hamming = stringdist::stringdist(tolower(username), tolower(other_name), method = "hamming"), #majority infinite values
      lcs = stringdist::stringdist(tolower(username), tolower(other_name), method = "lcs"),
      #qgram = stringdist::stringdist(tolower(username), tolower(other_name), method = "qgram"), high correlation with lcs method
      cosine = stringdist::stringdist(tolower(username), tolower(other_name), method = "cosine"),
      jaccard = stringdist::stringdist(tolower(username), tolower(other_name), method = "jaccard"),
      jw = stringdist::stringdist(tolower(username), tolower(other_name), method = "jw"),
      soundex = stringdist::stringdist(tolower(username), tolower(other_name), method = "soundex")
    )
    colnames(temp) <- paste0(prefix, colnames(temp))

    return(temp)
  }

  #######################
  ## Variable extraction

  username <- sub("@.*$", "", emails)

  #######################
  # general variables
  domain <- tolower(make.names(sub("^.*@", "", emails)))

  major_domain <- sub(".*\\.", "", domain)

  num_chars <- nchar(username)

  num_digits <- vapply(username, safeGregCount, pattern = "[0-9]", FUN.VALUE = integer(1))

  num_dots <- vapply(username, safeGregCount, pattern = "\\.", FUN.VALUE = integer(1))

  num_caps <- vapply(username, safeGregCount, pattern = "[A-Z]", FUN.VALUE = integer(1))

  total_letters <- vapply(username, safeTotalLetters, FUN.VALUE = integer(1))

  prop_caps <-num_caps / total_letters

  prop_digits <- num_digits / num_chars

  max_consecutive_digits <- vapply(username, function(x) {
    if (is.na(x)) return(NA_integer_)
    m <- gregexpr("[0-9]+", x, perl = TRUE)[[1]]
    if (m[1] == -1) {
      0L
    } else {
      # Extract the lengths of each match
      lengths <- attr(m, "match.length")
      max(lengths)
    }
  }, FUN.VALUE = integer(1))

  res <- data.frame(
    email_domain = domain,
    email_major_domain = major_domain,
    email_n_chars = num_chars,
    email_n_digits = num_digits,
    email_n_dots = num_dots,
    email_n_caps = num_caps,
    email_total_letters = total_letters,
    email_prop_digits = prop_digits,
    email_max_consecutive_digits = max_consecutive_digits
  )

  #######################
  # name and surname variables

  if(client_name_provided) {
    res$email_name_in_email <- as.numeric(mapply(
      function(u, nm) {
        if (is.na(u) || is.na(nm)) return(NA)
        grepl(pattern = tolower(nm), x = tolower(u), fixed = TRUE)
      },
      username, client_name
    ))

    # Calculated separately from the other name distances as Levenshtein has been included in the base R functions.
    res$email_name_in_email_dist_lv <- mapply(
      function(u, nm) {
        if (is.na(u) || is.na(nm)) return(NA)
        utils::adist(tolower(u), tolower(nm), ignore.case = TRUE)
      },
      username, client_name
    )
    if(!requireNamespace("stringdist", quietly = TRUE)) {
      warning("Package 'stringdist' is not installed.
             String distance not calculated.")
    } else {
      res <- cbind(
        res,
        calculate_string_distance(username, client_name, 'email_name_in_email_dist_')
      )
    }
  }

  if(client_surname_provided) {
    res$email_surname_in_email <- as.numeric(mapply(
      function(u, nm) {
        if (is.na(u) || is.na(nm)) return(NA)
        grepl(pattern = tolower(nm), x = tolower(u), fixed = TRUE)
      },
      username, client_surname
    ))

    # Calculated separately from the other name distances as Levenshtein has been included in the base R functions.
    res$email_surname_in_email_dist_lv <- mapply(
      function(u, nm) {
        if (is.na(u) || is.na(nm)) return(NA)
        utils::adist(tolower(u), tolower(nm), ignore.case = TRUE)
      },
      username, client_surname
    )

    if(!requireNamespace("stringdist", quietly = TRUE)) {
      warning("Package 'stringdist' is not installed.
             String distance not calculated.")
    } else {
      res <- cbind(
        res,
        calculate_string_distance(username, client_surname, 'email_surname_in_email_dist_')
      )
    }
  }

  if(client_name_provided && client_surname_provided) {
    res$email_fullname_in_email_dist_lv <- mapply(
      function(u, nm) {
        if (is.na(u) || is.na(nm)) return(NA)
        utils::adist(tolower(u), tolower(nm), ignore.case = TRUE)
      },
      username, full_name
    )
    if(!requireNamespace("stringdist", quietly = TRUE)) {
      warning("Package 'stringdist' is not installed.
             String distance not calculated.")
    } else {
      res <- cbind(
        res,
        calculate_string_distance(username, full_name, 'email_fullname_in_email_dist_')
      )
    }
  }

  #######################
  # date of birth variables
  if(client_dob_provided) {

    year_of_birth <- format(date_of_birth, "%Y")  # e.g. "1986"
    last_two_digits <- substring(year_of_birth, 3, 4)  # e.g. "86"
    month_of_birth <- format(date_of_birth, "%m")  # e.g. "10"
    day_of_birth <- format(date_of_birth, "%d")  # e.g. "09"

    res$email_has_full_year_of_birth <- as.numeric(mapply(function(u, y) {
      if (is.na(u) || is.na(y)) return(NA)
      grepl(pattern = y, x = tolower(u), fixed = TRUE)
    }, username, year_of_birth))

    res$email_has_last_two_digits_of_birth <- as.numeric(mapply(function(u, y) {
      if (is.na(u) || is.na(y)) return(NA)
      grepl(pattern = y, x = tolower(u), fixed = TRUE)
    }, username, last_two_digits))

    # Check if the username contains a "full date" (DOB) in multiple formats
    dob_regex <- mapply(function(y, m, d) {
      if (is.na(y) || is.na(m) || is.na(d)) return(NA)
      sprintf("%s[\\._-]?%s[\\._-]?%s", y, m, d)
    }, year_of_birth, month_of_birth, day_of_birth)

    res$email_has_full_dob_in_username <- as.numeric(mapply(function(u, dob_pat) {
      if (is.na(u) || is.na(dob_pat)) return(NA)
      grepl(dob_pat, tolower(u), ignore.case = TRUE, perl = FALSE)
    }, username, dob_regex))


    # Check if there's another 4-digit year in [1920..2020] not matching year_of_birth
    year_regex <- "\\b(19[2-9]\\d|20[0-1]\\d|2020)\\b"

    res$email_has_other_4digit_year <- as.numeric(mapply(function(u, y) {
      if (is.na(u) || is.na(y)) return(NA)
      # Find all matches
      matches <- gregexpr(pattern = year_regex, text = tolower(u), perl = TRUE)[[1]]
      if (matches[1] == -1) {
        return(FALSE)  # no match
      } else {
        matched_years <- regmatches(tolower(u), list(matches))
        any(matched_years != y)
      }
    }, username, year_of_birth))
  }

  return(res)
}
