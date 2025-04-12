#' Extract IP Address Features
#'
#' This function extracts a comprehensive set of features from a vector of IP address strings to support feature engineering in credit-scoring datasets. It processes both IPv4 and IPv6 addresses and returns a data frame with derived features. The features include IP version classification, octet-level breakdown for IPv4 addresses (with both string‐ and numeric-based octets), checks for leading zeros, a numeric conversion of the address, a basic approximation of IPv6 numeric values, pattern metrics such as a palindrome check and Shannon entropy, multicast status, and a Hilbert curve encoding for IPv4 addresses.
#'
#' @param ip_addresses A character vector of IP address strings.
#' @param error_on_invalid Logical flag indicating how to handle invalid IP addresses. If \code{TRUE}, the function throws an error upon encountering any invalid IP address; if \code{FALSE} (the default), invalid IP addresses are replaced with \code{NA} and a warning is issued.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{\code{ip_version}}{A character vector indicating the IP version; either \code{"IPv4"} or \code{"IPv6"}. Invalid addresses are set to \code{NA}.}
#'   \item{\code{ip_v4_octet1}}{The numeric conversion of the first octet of an IPv4 address as extracted from the IP string.}
#'   \item{\code{ip_v4_octet2}}{The numeric conversion of the second octet of an IPv4 address.}
#'   \item{\code{ip_v4_octet3}}{The numeric conversion of the third octet of an IPv4 address.}
#'   \item{\code{ip_v4_octet4}}{The numeric conversion of the fourth octet of an IPv4 address.}
#'   \item{\code{ip_v4_octet1_has_leading_zero}}{An integer flag indicating whether the first octet of an IPv4 address includes a leading zero.}
#'   \item{\code{ip_v4_octet2_has_leading_zero}}{An integer flag indicating whether the second octet includes a leading zero.}
#'   \item{\code{ip_v4_octet3_has_leading_zero}}{An integer flag indicating whether the third octet includes a leading zero.}
#'   \item{\code{ip_v4_octet4_has_leading_zero}}{An integer flag indicating whether the fourth octet includes a leading zero.}
#'   \item{\code{ip_leading_zero_count}}{An integer count of how many octets in an IPv4 address contain leading zeros.}
#'   \item{\code{ip_v4_numeric_vector}}{The 32-bit integer representation of an IPv4 address, computed as \eqn{(A * 256^3) + (B * 256^2) + (C * 256) + D}.}
#'   \item{\code{ip_v6_numeric_approx_vector}}{An approximate numeric conversion of an IPv6 address. This value is computed from the eight hextets and is intended for interval comparisons only; precision may be lost for large values (above 2^53).}
#'   \item{\code{ip_is_palindrome}}{An integer value indicating whether the entire IP address string is a palindrome (i.e., it reads the same forwards and backwards).}
#'   \item{\code{ip_entropy}}{A numeric value representing the Shannon entropy of the IP address string, computed over the distribution of its characters. Higher entropy values indicate a more varied (less repetitive) pattern.}
#' }
#'
#' @details
#' The function follows these steps:
#' \itemize{
#'   \item \strong{Validation:} Each IP address is checked against regular expressions for both IPv4 and IPv6. If an IP does not match either pattern, it is deemed invalid. Depending on the value of \code{error_on_invalid}, invalid entries are either replaced with \code{NA} (with a warning) or cause an error.
#'   \item \strong{IP Version Identification:} The function determines whether an IP address is IPv4 or IPv6.
#'   \item \strong{IPv4 Feature Extraction:}
#'     \itemize{
#'       \item The IPv4 addresses are split into four octets.
#'       \item For each octet, both the raw (string) and numeric representations are extracted.
#'       \item The presence of leading zeros is checked for each octet, and the total count of octets with leading zeros is computed.
#'       \item The full IPv4 address is converted to a 32-bit numeric value.
#'       \item Hilbert curve encoding is applied to the numeric value, yielding two dimensions that can be used as features in modeling.
#'     }
#'   \item \strong{IPv6 Feature Extraction:} For IPv6 addresses, an approximate numeric conversion is performed to allow for coarse interval analysis.
#'   \item \strong{Pattern Metrics:} Independent of IP version, the function computes:
#'     \itemize{
#'       \item A palindrome check on the entire IP string.
#'       \item The Shannon entropy of the IP string to capture the diversity of characters.
#'     }
#' }
#'
#' @examples
#' # Load the package's sample dataset
#' data(featForge_sample_data)
#'
#' # Extract IP features and combine them with the original IP column
#' result <- cbind(
#'   data.frame(ip = featForge_sample_data$ip),
#'   extract_ip_features(featForge_sample_data$ip)
#' )
#' print(result)
#'
#' @export
extract_ip_features <- function(ip_addresses, error_on_invalid = FALSE) {

  #######################
  ## Basic input checks

  # Regex for IPv4: e.g., 1.2.3.4, up to 255.255.255.255
  ipv4_pattern <- "^((25[0-5]|2[0-4]\\d|[01]?\\d?\\d)\\.){3}(25[0-5]|2[0-4]\\d|[01]?\\d?\\d)$"

  # Regex for (simple) IPv6:
  # This won't perfectly cover all complex IPv6 compressions, but covers the common cases
  ipv6_pattern <- "^([0-9A-Fa-f]{1,4}:){1,7}[0-9A-Fa-f]{1,4}$"

  # Determine which entries match IPv4 or IPv6
  is_ipv4 <- grepl(ipv4_pattern, ip_addresses, ignore.case = TRUE)
  is_ipv6 <- grepl(ipv6_pattern, ip_addresses, ignore.case = TRUE)

  invalid_indices <- which(!(is_ipv4 | is_ipv6))

  if (length(invalid_indices) > 0) {
    if (error_on_invalid) {
      stop(
        "Invalid IP addresses found. ",
        "Set `error_on_invalid = FALSE` to replace invalid values with NA instead."
      )
    } else {
      ip_addresses[invalid_indices] <- NA

      warning(
        "Some invalid IP addresses were replaced with NA. ",
        "Set `error_on_invalid = TRUE` to throw an error instead."
      )
    }
  }

  #######################
  ## Helper functions
  expand_ipv6 <- function(addr) {
    parts <- unlist(strsplit(addr, ":", fixed = TRUE))

    # Count how many blank parts are present (which indicate compression)
    # e.g. "2001:db8::1" => c("2001", "db8", "", "1")
    # The empty string between "db8" and "1" indicates omitted groups.
    empty_indices <- which(nchar(parts) == 0)

    if (length(empty_indices) > 0) {

      n_groups <- length(parts)
      n_nonempty <- sum(nchar(parts) > 0)

      n_missing <- 8 - n_nonempty

      insert_at <- empty_indices[1]
      parts <- parts[nchar(parts) > 0]  # remove empty
      parts <- append(parts, rep("0", n_missing), after = insert_at - 1)
    }

    # Now pad each part to 4 hex digits
    parts_padded <- sapply(parts, function(x) {
      sprintf("%04s", toupper(x))
    })

    parts_padded
  }

  is_palindrome_ip <- function(ip_str) {
    chars <- strsplit(ip_str, "")[[1]]    # split into characters
    reversed <- paste0(rev(chars), collapse = "")
    identical(ip_str, reversed)
  }

  shannon_entropy_ip <- function(ip_str) {
    chars <- unlist(strsplit(ip_str, ""))   # split into individual characters
    # compute frequency of each unique character
    freq <- table(chars) / length(chars)
    # sum of -p * log2(p) across all unique characters
    -sum(freq * log2(freq))
  }


  #######################
  ## General IP features

  res <- data.frame(
    ip_version = ifelse(is.na(ip_addresses), NA_character_,
                        ifelse(is_ipv4, "IPv4",
                               ifelse(is_ipv6, "IPv6", NA_character_)))
  )

  #######################
  ## Octet level features
  octet_list <- strsplit(ip_addresses[is_ipv4], "\\.")

  if(length(octet_list) > 0) {
    octet_matrix <- do.call(rbind, lapply(octet_list, as.integer))

    leading_zero_matrix <- do.call(rbind, lapply(octet_list, function(x) {
      sapply(x, function(oct) {
        nchar(oct) > 1 && substr(oct, 1, 1) == "0"
      })
    }))

    res$ip_v4_octet_1 <- NA_integer_
    res$ip_v4_octet_2 <- NA_integer_
    res$ip_v4_octet_3 <- NA_integer_
    res$ip_v4_octet_4 <- NA_integer_

    res$ip_v4_octet_1_has_leading_zero <- NA_integer_
    res$ip_v4_octet_2_has_leading_zero <- NA_integer_
    res$ip_v4_octet_3_has_leading_zero <- NA_integer_
    res$ip_v4_octet_4_has_leading_zero <- NA_integer_

    res$ip_v4_octet_1[is_ipv4] <- octet_matrix[, 1]
    res$ip_v4_octet_2[is_ipv4] <- octet_matrix[, 2]
    res$ip_v4_octet_3[is_ipv4] <- octet_matrix[, 3]
    res$ip_v4_octet_4[is_ipv4] <- octet_matrix[, 4]

    res$ip_v4_octet_1_has_leading_zero[is_ipv4] <- as.numeric(leading_zero_matrix[, 1])
    res$ip_v4_octet_2_has_leading_zero[is_ipv4] <- as.numeric(leading_zero_matrix[, 2])
    res$ip_v4_octet_3_has_leading_zero[is_ipv4] <- as.numeric(leading_zero_matrix[, 3])
    res$ip_v4_octet_4_has_leading_zero[is_ipv4] <- as.numeric(leading_zero_matrix[, 4])

    res$ip_leading_zero_count[is_ipv4] <- rowSums(leading_zero_matrix)
  }

  #######################
  ## Numeric conversions
  #v4

  if(any(is_ipv4)) {
    res$ip_v4_numeric_vector <- NA_integer_

    res$ip_v4_numeric_vector[is_ipv4] <- (
      octet_matrix[, 1] * 256^3 +
        octet_matrix[, 2] * 256^2 +
        octet_matrix[, 3] * 256 +
        octet_matrix[, 4]
    )
  }

  if(any(is_ipv6)) {
    #v6
    # Important: This snippet assumes FULL (uncompressed) IPv6 strings (8 groups).
    # WARNING: For large IPv6 addresses beyond 2^53, you lose precision.
    hextet_list <- strsplit(ip_addresses[is_ipv6], ":")

    hextet_matrix <- do.call(rbind, lapply(hextet_list, function(x) {
      as.integer(strtoi(x, base = 16L))
    }))

    res$ip_v6_numeric_approx_vector <- NA_integer_
    res$ip_v6_numeric_approx_vector[is_ipv6] <- apply(hextet_matrix, 1, function(row) {
      val <- 0
      for (k in 1:8) {
        val <- val * 65536 + row[k]
      }
      val
    })
  }


  #######################
  ## palindrome/symmetry check

  res$ip_is_palindrome <- NA

  res$ip_is_palindrome[-invalid_indices] <- as.numeric(sapply(
    ip_addresses[-invalid_indices],
    is_palindrome_ip
  ))


  #######################
  ## palindrome/symmetry check

  res$ip_entropy <- NA_real_

  res$ip_entropy[-invalid_indices] <- sapply(
    ip_addresses[-invalid_indices],
    shannon_entropy_ip
  )


  #######################
  ## iptools package related features

  ## Commented out as iptools package is not available at the moment

  # #'   \item{\code{ip_is_multicast}}{An integer flag that indicates if the IP address falls within a multicast range.}
  # #'   \item{\code{ip_v4_hilbert_dim_1}}{The first coordinate (dimension 1) from applying Hilbert curve encoding to the numeric IPv4 address. This two-dimensional representation helps preserve the spatial locality of the IP addresses.}
  # #'   \item{\code{ip_v4_hilbert_dim_2}}{The second coordinate (dimension 2) from the Hilbert curve encoding of the IPv4 address.}

  # if(!requireNamespace("iptools", quietly = TRUE)) {
  #   warning("Package 'iptools' is not installed.
  #            Several ip-features will not be created.")
  # } else {
  #
  #   res$ip_is_multicast <- as.numeric(iptools::is_multicast(ip_addresses))
  #
  #   if(any(is_ipv4)) {
  #     temp <- iptools::hilbert_encode(res$ip_v4_numeric_vector[is_ipv4])
  #
  #     res$ip_v4_hilbert_dim_1 <- NA
  #     res$ip_v4_hilbert_dim_2 <- NA
  #
  #     res$ip_v4_hilbert_dim_1[is_ipv4] <- temp[, 1]
  #     res$ip_v4_hilbert_dim_2[is_ipv4] <- temp[, 2]
  #
  #   }
  # }

  return(res)
}
