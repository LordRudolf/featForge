
test_that("Inconsistencies in transaction dates in the `aggregate_applications` function", {

  test_data <- data.frame(
    id = rep(1, 4),
    obs_date = rep(as.Date('2025-01-01'), 4),
    window_start = rep(as.Date('2024-12-01'), 4),
    date = as.Date(c(
      '2024-01-01', '2024-12-31', '2025-01-01', '2025-01-02'
    )),
    value = c(1, 10, 100, 1000)
  )

  # transactions are later than scrape date
  expect_error({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'monthly',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )
  })

  # Expecting 0 as the few observed transaction dates matches with window_start and/or scrape dates
  expect_identical({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'weekly',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 0)

  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'all',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 1111)

  #whether it works without showing arguments
  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'all'
                           #observation_window_start_col = 'window_start',
                           #scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 1111)

  # But anything other period == 'all' shall throw an error if some of the argument columns are not provided
  expect_error({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'monthly',
                           #observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )
  })
  expect_error({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'monthly',
                           observation_window_start_col = 'window_start'
                           #scrape_date_col = 'obs_date'
    )
  })

  # Expect 110 as 100 happened on the same date as the scrape date and 10 a day before
  expect_identical({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = c(1, 1),
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 110)

  expect_identical({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = c(365, 1),
                           #observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 110)

  expect_identical({
    aggregate_applications(test_data[1:3, ],
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = c(366, 1),
                           #observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 111)
})


test_that("Inconsistencies in daily/weekly/monthly `aggregate_applications` function", {

  test_data <- data.frame(
    id = rep(1, 4),
    obs_date = rep(as.Date('2025-02-13'), 4),
    window_start = rep(as.Date('2024-11-13'), 4),
    date = as.Date(c(
      '2024-11-30', '2024-12-31', '2025-02-02', '2025-02-12'
    )),
    value = c(1, 10, 100, 1000)
  )
  #Note that obs date '2025-02-13' starts at mid-month and is Wednesday (mid week)

  #Expect 10 other transactions are not within-full-months intervals
  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'monthly',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 10)

  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'weekly',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 111)

  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'daily',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date'
    )[['sum_of_value']]
  }, 1111)
})


test_that("Dropped applications in the function `aggegate_applications`", {

  test_data <- data.frame(
    id = c(1, 1, 2, 3),
    obs_date = rep(as.Date('2025-01-01'), 4),
    window_start = rep(as.Date('2024-12-01'), 4),
    date = as.Date(c(
      '2024-01-01', '2024-12-31', '2024-01-01', '2024-12-02'
    )),
    value = c(1, 1, 1, 1)
  )

  expect_equal({
    nrow(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum
                             ),
                             period = 'daily',
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date'
      )
    )
  }, 3)

  expect_equal({
    nrow(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum
                             ),
                             period = c(1, 3),
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date'
      )
    )
  }, 3)
})

test_that("Period aggregation functions are not working correctly in the function `aggregate_applicaitons`", {

  test_data <- data.frame(
    id = rep(1, 4),
    obs_date = rep(as.Date('2025-01-15'), 4),
    window_start = rep(as.Date('2025-01-01'), 4),
    date = as.Date(c(
      '2025-01-02', '2025-01-05', '2025-01-05', '2025-01-10'
    )),
    value = c(10, 10, 0.5, -5)
  )

  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'daily',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date',
                           period_agg = length
    )[['sum_of_value']]
  }, 4)

  expect_identical({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'daily',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date',
                           period_agg = function(x) length(x[x > 0])
    )[['sum_of_value']]
  }, 3)

  # From '2025-01-01' to '2025-01-15' is 15-day period, the function `daily`, excludes the threshold days - we are left with 13 days.
  # We have observations known for 3 different days ('2025-01-02', '2025-01-05', and '2025-01-10'), leaving us with 10 unobserved days.
  # We assign 100 to the missing days, giving us expected sum of 10 * 100 + 10 + 10 +0.5 - 5 = 1015.5
  expect_equal({
    aggregate_applications(test_data,
                           id_col = 'id',
                           amount_col = 'value',
                           time_col = 'date',
                           ops = list(
                             sum_of_value = sum
                           ),
                           period = 'daily',
                           observation_window_start_col = 'window_start',
                           scrape_date_col = 'obs_date',
                           period_agg = sum,
                           period_missing_inputs = 100
    )[['sum_of_value']]
  }, 1015.5)
})

test_that("ops argument is not working correctly in the function `aggreage_applications`", {

  test_data <- data.frame(
    id = rep(1, 4),
    obs_date = rep(as.Date('2025-01-15'), 4),
    window_start = rep(as.Date('2025-01-01'), 4),
    date = as.Date(c(
      '2025-01-02', '2025-01-05', '2025-01-05', '2025-01-10'
    )),
    value = c(10, 0, -10, -10),
    cat = c('x1', 'x1', 'x2', 'x1'),
    cat2 = c('y1', 'y2', 'y1', 'y2')
  )

  expect_equal({
    as.vector(unlist(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum,
                               sum_of_positives = function(x) sum(x[x > 0])
                             ),
                             period = 'daily',
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date',
      )[1, 2:3]
    ))
  }, c(-10, 10))

  expect_equal({
    colnames(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum
                             ),
                             period = 'daily',
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date',
      )
    )[[2]]
  }, 'sum_of_value')

  expect_equal({
    as.vector(unlist(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum
                             ),
                             period = 'daily',
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date',
                             group_cols = c('cat', 'cat2')
      )[1, 2:5]
    ))
  }, c(0, -10, 0, -10))

  expect_equal({
    colnames(
      aggregate_applications(test_data,
                             id_col = 'id',
                             amount_col = 'value',
                             time_col = 'date',
                             ops = list(
                               sum_of_value = sum
                             ),
                             period = 'daily',
                             observation_window_start_col = 'window_start',
                             scrape_date_col = 'obs_date',
                             group_cols = c('cat', 'cat2')
      )
    )[2:5]
  }, c( "sum_of_value_cat_x1", "sum_of_value_cat_x2", "sum_of_value_cat2_y1", "sum_of_value_cat2_y2"))
})

