% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_psd2_features.R
\name{extract_keyword_features}
\alias{extract_keyword_features}
\title{Extract Keyword Features from Text Descriptions}
\usage{
extract_keyword_features(
  descriptions,
  min_freq = 1,
  use_matrix = TRUE,
  convert_to_df = TRUE
)
}
\arguments{
\item{descriptions}{A character vector of text descriptions.}

\item{min_freq}{A numeric value specifying the minimum frequency a token must have to be included in the output. Default is 1.}

\item{use_matrix}{Logical. If \code{TRUE} (the default), a sparse matrix is created using the Matrix package.}

\item{convert_to_df}{Logical. If \code{TRUE} (the default) and \code{use_matrix} is \code{TRUE}, the resulting sparse matrix is converted
to a dense data.frame. This is useful for binding with other data.frames, but may be memory-intensive if many unique tokens exist.}
}
\value{
Either a data.frame or a sparse matrix (if \code{convert_to_df} is \code{FALSE}) with one row per description
and one column per keyword. Each cell is binary (1 if the keyword is present, 0 otherwise).
}
\description{
This function processes a vector of text descriptions (e.g., transaction descriptions) and extracts binary keyword features.
Each unique word (token) that meets or exceeds the specified minimum frequency is turned into a column that indicates its presence (1)
or absence (0) in each description. For efficiency, the function can create a sparse matrix using the Matrix package.
Additionally, the user can choose to convert the output to a standard data.frame.
}
\details{
The function performs the following steps:
\enumerate{
  \item Converts the input text to lowercase and removes digits.
  \item Splits the text on any non-letter characters to extract tokens.
  \item Constructs a vocabulary from all tokens that appear at least \code{min_freq} times.
  \item Depending on the \code{use_matrix} flag, builds either a sparse matrix or a dense matrix of binary indicators.
  \item If \code{use_matrix} is \code{TRUE} and \code{convert_to_df} is also \code{TRUE}, the sparse matrix is converted to a data.frame.
}
}
\section{Warnings}{

\itemize{
  \item If a very large number of descriptions (e.g., more than 100,000) is provided with \code{use_matrix = FALSE}, a warning is issued because
        using a dense matrix may exhaust memory.
  \item If \code{use_matrix = TRUE}, \code{convert_to_df = TRUE}, and \code{min_freq} is set to 1 (i.e., no filtering), a warning is issued because converting
        a very large sparse matrix to a dense data.frame may result in an enormous object.
}
}

\examples{
# Load the sample transactions data.
data(featForge_transactions)

# Combine the transactions data with extracted keyword features.
trans <- cbind(featForge_transactions,
               extract_keyword_features(featForge_transactions$description))
head(trans) # we see that there have been added categories named "casino" and "utilities".

# Aggregate the number of transactions on the application level
# by summing the binary keyword indicators for the 'casino' and
# 'utilities' columns, using a 30-day period.
aggregate_applications(
  trans,
  id_col = "application_id",
  amount_col = "amount",
  group_cols = c("casino", "utilities"),
  time_col = "transaction_date",
  scrape_date_col = "scrape_date",
  ops = list(times_different_transactions_in_last_30_days = sum),
  period_agg = length,
  period = c(30, 1)
)

}
