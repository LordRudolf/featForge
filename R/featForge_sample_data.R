# featForge_sample_data <- data.frame(
#
#   application_id = 1:30,
#
#   application_created_at =  as.POSIXct(c(
#     "2022-01-05 08:30:00",
#     "2022-01-20 12:45:00",
#     "2022-02-10 09:15:00",
#     "2022-02-28 17:00:00",
#     "2022-03-15 11:00:00",
#     "2022-04-01 14:30:00",
#     "2022-04-20 18:15:00",
#     "2022-05-05 07:45:00",
#     "2022-05-20 16:00:00",
#     "2022-06-10 10:30:00",
#     "2022-07-01 13:00:00",
#     "2022-07-20 20:15:00",
#     "2022-08-15 06:45:00",
#     "2022-09-05 19:30:00",
#     "2022-10-01 22:00:00",
#     "2023-01-10 08:00:00",
#     "2023-01-25 12:30:00",
#     "2023-02-15 09:45:00",
#     "2023-03-05 15:00:00",
#     "2023-03-20 17:30:00",
#     "2023-04-10 11:15:00",
#     "2023-04-25 13:45:00",
#     "2023-05-10 08:30:00",
#     "2023-05-25 16:00:00",
#     "2023-06-15 10:00:00",
#     "2023-07-01 14:15:00",
#     "2023-07-20 19:45:00",
#     "2023-08-05 07:30:00",
#     "2023-09-10 18:00:00",
#     "2023-10-01 21:30:00"
#   )),
#
#   client_name = c(
#     "John", "Lisa", "Michael", "Sarah", "Tom",
#     "Emily", "Robert", "Charlotte", "Andrew", "Victoria",
#     "David", "Linda", "Henry", "Jessica", "Mark",
#     "Anna", "Martin", "Sophie", "Ethan", "Rachel",
#     "Oliver", "Paula", "Kevin", "Samantha", "Gregory",
#     "Megan", "Roger", "Harriet", "Tyler", "Nick"
#   ),
#   client_surname = c(
#     "Smith", "Adams", "Johnson", "Oneill", "Brown",
#     "White", "King", "Dawson", "Martinez", "Hill",
#     "Lo", "Novak", "Oreilly", "Black", "Stone",
#     "Wolf", "Garfield", "Taylor", "Evans", "Johnson",
#     "Green", "Young", "Lee", "Harris", "Adams",
#     "Lister", "Davis", "Johnson", "Obrien", "Wayne"
#   ),
#   date_of_birth = as.Date(c(
#     "1989-10-09", "1975-01-15", "1992-12-01", "1990-06-30", "1980-03-17",
#     "1970-12-31", "1965-08-23", "2000-01-01", "1955-05-05", "1988-11-11",
#     "1981-09-09", "1960-07-15", "1945-01-30", "1978-05-25", "1977-04-04",
#     "1993-03-15", "1968-10-10", "1996-02-28", "1950-12-12", "1984-07-07",
#     "1991-10-31", "1966-12-24", "1999-08-08", "1985-10-29", "1973-06-19",
#     "1994-01-11", "1959-09-30", "1971-01-02", "1998-03-03", "1982-02-14"
#   )),
#   email = c(
#     #  1 John        1989 -> No digits, not referencing name
#     "pineapplefan@hotmail.com",
#     #  2 Lisa        1975 -> Contains "Lisa" but uses '1980' (mismatch)
#     "Lisa_1980Ad@domain.co.uk",
#     #  3 Michael     1992 -> Partial name, no birth-year reference
#     "mike_j@somewhere.org",
#     #  4 Sarah       1990 -> Partial surname, uses '1991' (mismatch)
#     "S.Oneill_1991@random.io",
#     #  5 Tom         1980 -> Partial name + random digits, no direct DOB link
#     "tom_111@brownmail.com",
#     #  6 Emily       1970 -> Full name, no digits
#     "emilywhite@some-domain.net",
#     #  7 Robert      1965 -> Surname reference, no digits
#     "drummerking@my.org",
#     #  8 Charlotte   2000 -> Year reference matches DOB
#     "char_2000d@domain.com",
#     #  9 Andrew      1955 -> Contains year '55'
#     "Andrew55Martinez@something.com",
#     # 10 Victoria    1988 -> Partial name + '88' matches DOB
#     "v.hill88@hillmail.com",
#     # 11 David       1981 -> Full name, no digits
#     "davidlo@fancy.com",
#     # 12 Linda       1960 -> Partial name + correct birth year
#     "LN1960_novak@yahoo.co.uk",
#     # 13 Henry       1945 -> Contains '45' (DOB last two digits)
#     "henry_oreilly45@ggmail.com",
#     # 14 Jessica     1978 -> Full name, no digits
#     "jessblack@hotmail.net",
#     # 15 Mark        1977 -> Partial name + partial year
#     "Mark_77@somewhere.org",
#     # 16 Anna        1993 -> Has full year + partial surname
#     "anna1993wolf@wolfpack.com",
#     # 17 Martin      1968 -> References '68'
#     "martin_68@random.co",
#     # 18 Sophie      1996 -> Uses '1995' (mismatch)
#     "S.T.1995@domain.org",
#     # 19 Ethan       1950 -> Surname only, no digits
#     "eevans@evans.io",
#     # 20 Rachel      1984 -> Partial name + '90' mismatch
#     "r.johnson_90@domain.com",
#     # 21 Oliver      1991 -> Full name, no digits
#     "OllyGreen@ghostmail.net",
#     # 22 Paula       1966 -> Surname + partial year (66) matches
#     "young_paula66@oldmail.com",
#     # 23 Kevin       1999 -> Partial name + '98' mismatch
#     "k.lee_98@site.co.uk",
#     # 24 Samantha    1985 -> Name only, no digits
#     "Sam_harris@any.com",
#     # 25 Gregory     1973 -> Name + '2010' mismatch
#     "greg_2010@coolstuff.info",
#     # 26 Megan       1994 -> Name only, no digits
#     "Mlister@lister.org",
#     # 27 Roger       1959 -> Partial name + partial year
#     "rogerdav_59@someemail.com",
#     # 28 Harriet     1971 -> '1972' mismatch in email
#     "hjohnson1972@johnsonfamily.org",
#     # 29 Tyler       1998 -> '2000' mismatch
#     "TyOb_2000@mymail.com",
#     # 30 Nick        1982 -> Surname + correct birth year
#     "NWayne1982@lovemail.com"
#   ),
#
#   ip = c(
#     # 1) Valid IPv4
#     "192.168.0.1",
#     "10.0.0.1",
#     "8.8.8.8",
#     "255.255.255.255",
#     "127.0.0.1",
#     "0.0.0.0",
#     "172.16.5.10",
#     "198.51.100.42",
#     "203.0.113.99",
#     "192.168.001.001", # leading zeros
#     "1.1.1.1",
#     "250.250.250.250",
#
#     # 2) Valid IPv6 (mixed compressed/uncompressed)
#     "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
#     "::1",
#     "2001:db8:85a3::8a2e:370:7334",
#     "fe80::f2de:f1ff:fe14:abcd",
#     "2001:4860:4860::8888",
#     "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", # max IPv6
#     "1234:5678:90ab:cdef:0000:0000:abcd:ef01",
#     "0000:0000:0000:0000:0000:0000:0000:0001", # all leading zeros
#     "2607:f8b0:4005:0802:0000:0000:0000:200e",
#
#     # 3) Invalid or problematic
#     "999.999.999.999",     # invalid IPv4
#     "300.168.0.1",         # invalid IPv4, 300 > 255
#     "192.168.0.256",       # invalid IPv4, 256 > 255
#     "2001:::85a3::370:7334", # malformed IPv6 (too many colons)
#     "1234::abcde",         # invalid hextet (5 hex digits)
#     "",                    # empty string
#     "hello world",         # random non-IP string
#     NA,                    # missing
#     "   "                  # whitespace-only
#   ),
#
#   stringsAsFactors = FALSE
# )
# usethis::use_data(sample_data, overwrite = TRUE)



#' Sample Data for Package Testing and Demonstration
#'
#' A dataset containing auto-generated values by ChatGPT o3-mini-high for package testing and demonstration purposes.
#' This example dataset consists of 30 rows and 7 variables that illustrate the structure
#' of a typical application record.
#'
#' The dataset includes the following variables:
#'
#' \describe{
#'   \item{application_id}{A unique numeric identifier for each application.}
#'   \item{application_created_at}{The date and time when the application was created.}
#'   \item{client_name}{The first name of the client.}
#'   \item{client_surname}{The last name of the client.}
#'   \item{date_of_birth}{The client’s date of birth.}
#'   \item{email}{The client’s email address.}
#'   \item{ip}{The IP address associated with the client.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name featForge_sample_data
#' @usage data(featForge_sample_data)
#' @format A data frame with 30 rows and 7 variables.
#'
#' @details
#' This dataset was automatically generated for the purpose of testing and demonstrating
#' the package functionality. The values (e.g., dates, emails, and IP addresses) are illustrative
#' and do not represent actual user data.
#'
#' @examples
#' # Load the dataset
#' data(featForge_sample_data)
#'
#' # Display the first few rows of the dataset
#' head(featForge_sample_data)
"featForge_sample_data"



##########################################################################################################
# set.seed(123)
# n_trans <- 250
# # Sample application_ids from the base dataset ensuring at least 20 unique applications
# app_ids <- sample(featForge_sample_data$application_id, n_trans, replace = TRUE)
# while(length(unique(app_ids)) < 20) {
#   app_ids <- sample(featForge_sample_data$application_id, n_trans, replace = TRUE)
# }
# # Create a basic transactions dataframe with application_id
# trans <- data.frame(application_id = app_ids, stringsAsFactors = FALSE)
#
# # Merge with the base dataset to get each application's creation timestamp
# trans <- merge(trans, featForge_sample_data[, c("application_id", "application_created_at")],
#                by = "application_id", all.x = TRUE)
#
# # Generate transaction_date for each row:
# # It will be a random timestamp between (application_created_at - 180 days) and application_created_at.
# trans$transaction_date <- as.Date(as.POSIXct(sapply(trans$application_created_at, function(app_date) {
#   start_date <- app_date - 180 * 24 * 3600  # 180 days before
#   as.POSIXct(runif(1, as.numeric(start_date), as.numeric(app_date)), origin = "1970-01-01")
# }), origin = "1970-01-01"))
#
# # Simulate transaction amounts:
# # 70% chance for outgoing (negative amount) and 30% for incoming (positive amount)
# trans$amount <- sapply(1:nrow(trans), function(i) {
#   if (runif(1) < 0.7) {
#     round(runif(1, 50, 500), 2) * -1
#   } else {
#     round(runif(1, 100, 1000), 2)
#   }
# })
#
# # Introduce similar outgoing amounts for some applications in the same month.
# regular_apps <- sample(unique(trans$application_id), 5)
# trans$month <- format(trans$transaction_date, "%Y-%m")
# for (app in regular_apps) {
#   idx_app <- which(trans$application_id == app & trans$amount < 0)
#   if(length(idx_app) > 0) {
#     for (m in unique(trans$month[idx_app])) {
#       idx_month <- which(trans$application_id == app & trans$month == m & trans$amount < 0)
#       if(length(idx_month) > 1) {
#         base_val <- round(runif(1, 20, 200), 2)
#         trans$amount[idx_month] <- -abs(base_val)
#       }
#     }
#   }
# }
# trans$month <- NULL  # Remove temporary month column
#
# # Generate transaction category.
# categories <- c("groceries", "salary", "gambling", "utilities", "travel",
#                 "entertainment", "rent", "shopping")
# trans$category <- sample(categories, nrow(trans), replace = TRUE)
#
# # Helper function to generate a description based on transaction category.
# generate_description <- function(category) {
#   if (category == "gambling") {
#     type <- sample(1:5, 1)
#     desc <- switch(as.character(type),
#                    "1" = {
#                      bank_names <- c("AlphaBank", "BetaBank", "GammaFinance", "DeltaTrust", "EpsilonCredit")
#                      name <- sample(bank_names, 1)
#                      num <- sample(1000:9999, 1)
#                      paste(name, num)
#                    },
#                    "2" = {
#                      bank_names <- c("OmegaBank", "SigmaFinance", "ZetaTrust", "KappaCredit")
#                      name <- sample(bank_names, 1)
#                      num <- sample(100:999, 1)
#                      special <- sample(c("@", "#", "$", "%", "&"), 1)
#                      paste(name, special, num, sep = "")
#                    },
#                    "3" = {
#                      adjectives <- c("Quick", "Silent", "Brave", "Lucky", "Fierce", "Mighty")
#                      nouns <- c("transaction", "transfer", "payment", "remittance", "deposit", "withdrawal")
#                      num <- sample(10000:99999, 1)
#                      paste(sample(adjectives, 1), sample(nouns, 1), "code", num)
#                    },
#                    "4" = {
#                      sentences <- c("Payment sent for invoice", "Transferred funds for service fee",
#                                     "Money sent for rent", "Payment made for utilities", "Funds transferred as agreed")
#                      num <- sample(10:999, 1)
#                      paste(sample(sentences, 1), num)
#                    },
#                    "5" = {
#                      gambling_templates <- c("Casino win processed, ticket", "Bet placed at casino",
#                                              "Casino payout completed, ref", "Gambling expense at casino",
#                                              "Casino deposit confirmed")
#                      num <- sample(100:999, 1)
#                      paste(sample(gambling_templates, 1), num)
#                    }
#     )
#     # Ensure the word "casino" appears in the description.
#     if (!grepl("casino", desc, ignore.case = TRUE)) {
#       desc <- paste(desc, "casino")
#     }
#     return(desc)
#   } else {
#     type <- sample(1:5, 1)
#     desc <- switch(as.character(type),
#                    "1" = {
#                      bank_names <- c("AlphaBank", "BetaBank", "GammaFinance", "DeltaTrust", "EpsilonCredit")
#                      name <- sample(bank_names, 1)
#                      num <- sample(1000:9999, 1)
#                      paste(name, num)
#                    },
#                    "2" = {
#                      bank_names <- c("OmegaBank", "SigmaFinance", "ZetaTrust", "KappaCredit")
#                      name <- sample(bank_names, 1)
#                      num <- sample(100:999, 1)
#                      special <- sample(c("@", "#", "$", "%", "&"), 1)
#                      paste(name, special, num, sep = "")
#                    },
#                    "3" = {
#                      adjectives <- c("Quick", "Silent", "Brave", "Lucky", "Fierce", "Mighty")
#                      nouns <- c("transaction", "transfer", "payment", "remittance", "deposit", "withdrawal")
#                      num <- sample(10000:99999, 1)
#                      paste(sample(adjectives, 1), sample(nouns, 1), "code", num)
#                    },
#                    "4" = {
#                      sentences <- c("Payment sent for invoice", "Transferred funds for service fee",
#                                     "Money sent for rent", "Payment made for utilities", "Funds transferred as agreed")
#                      num <- sample(10:999, 1)
#                      paste(sample(sentences, 1), num)
#                    },
#                    "5" = {
#                      unique_words <- c("Nebula", "Orbit", "Quantum", "Fusion", "Vertex", "Crest", "Pulse")
#                      num <- sample(1000:9999, 1)
#                      paste(paste(sample(unique_words, sample(2:4, 1), replace = FALSE), collapse = " "), "transaction", num)
#                    }
#     )
#     return(desc)
#   }
# }
# # Generate description for each transaction based on its category.
# trans$description <- sapply(trans$category, generate_description)
#
# trans$scrape_date <- as.Date(trans$application_created_at)
# trans$obs_start <- featForge_transactions$scrape_date - 180
#
# # Final transactions dataset with selected columns.
# featForge_transactions <- trans[, c("application_id", "scrape_date", "obs_start", "transaction_date", "amount", "description", "category")]
# usethis::use_data(featForge_transactions, overwrite = TRUE)


#' Transactions Data for Package Testing and Demonstration
#'
#' A dataset containing auto-generated banking transactions for package testing and demonstration purposes.
#' This example dataset consists of over 200 rows and 5 variables that illustrate the structure of a typical
#' transaction record associated with an application.
#'
#' The dataset includes the following variables:
#'
#' \describe{
#'   \item{application_id}{A numeric identifier linking the transaction to the corresponding application.}
#'   \item{scrape_date}{The end date of the transactions observation window. Assumed to be on the same date with the application's creation date.}
#'   \item{obs_start}{The start date of the transactions observation window. Assumed to be 180 days apart with the scrape_date.}
#'   \item{transaction_date}{The date and time when the transaction occurred. The date is always on or before the associated scrape_date (application's creation date) and on or after the obs_start date.}
#'   \item{amount}{The monetary amount of the transaction. Negative values indicate outgoing transactions, while positive values indicate incoming transactions.}
#'   \item{description}{A detailed description of the transaction. Descriptions vary in style and content, including combinations of generated institution names, random numbers, special characters, creative phrases, and full English sentences. For transactions with a category of "gambling", the description always includes the word "casino".}
#'   \item{category}{The transaction category, such as groceries, salary, gambling, utilities, travel, entertainment, rent, or shopping.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name featForge_transactions
#' @usage data(featForge_transactions)
#' @format A data frame with over 200 rows and 5 variables.
#'
#' @details
#' This dataset was automatically generated for the purpose of testing and demonstrating the package functionality.
#' The transactions simulate realistic banking activity linked to application records. The transaction dates are
#' generated in relation to each application's creation date, ensuring logical consistency in the timeline, while the
#' descriptions have been enriched to include a wide variety of content, with special attention given to transactions
#' in the gambling category.
#'
#' @examples
#' # Load the transactions dataset
#' data(featForge_transactions)
#'
#' # Display the first few rows of the dataset
#' head(featForge_transactions)
"featForge_transactions"
