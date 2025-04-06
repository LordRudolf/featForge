# featForge

featForge is an R package for feature engineering tailored to credit scoring and application data analysis. It provides a suite of functions to extract, transform, and aggregate features from various data sources—including timestamps, email addresses, IP addresses, and transactional.

## Features

- **Cyclical Transformations:**  
  Convert cyclic variables (like hours, days, and months) into sine and cosine representations. These transformations help machine learning models understand the inherent cyclical nature of time-based features.  
  *(See functions in `utils.R`)*

- **Timestamp Feature Extraction:**  
  Extract detailed date and time features from timestamps, including month, day, week, and hour, along with their cyclical transformations. Additional features include client age and the number of days until the next birthday.  
  *(See `extract_timestamp_features.R`)*

- **Email Feature Extraction:**  
  Parse and analyze email addresses to extract domains, username characteristics, and string distance metrics, which are useful for credit scoring and fraud detection.  
  *(See `extract_email_features.R`)*

- **IP Address Feature Extraction:**  
  Process both IPv4 and IPv6 addresses to derive a rich set of features, such as octet-level breakdowns, numeric conversions, entropy measures, and spatial encoding via Hilbert curves.  
  *(See `extract_ip_features.R`)*

- **Data Aggregation:**  
  Aggregate numeric data over specified time periods (daily, weekly, monthly, or custom cycles) and compute summary statistics, enabling you to capture trends and patterns over time.  
  *(See `aggregate_applications.R`)*

- **Sample Data:**  
  Use included sample datasets like `featForge_sample_data` and `featForge_transactions` to quickly test and demonstrate the package functionality.

## Installation

Install the development version of featForge directly from GitHub with:

```r
devtools::install_github("LordRudolf/featForge")
