test_that("Incorrect feature values from the function extract_basic_description_features", {
  descs <- c("AAAAAAAA...",
             "Transferred funds for service fee 990",
             "Mighty remittance code 99816 casino",
             NA)

  res <- extract_basic_description_features(descs)

  expect_true(all(is.na(res[4, ])))
  expect_true(res$has_digits[[1]] == 0)
  expect_true(res$n_digits[[2]] == 3)
  expect_true(res$n_special[[1]] == 3)
  expect_true(res$n_punct[[1]] == 3)
  expect_true(res$n_words[[2]] == 6)
  expect_true(res$n_chars[[1]] == 11)
  expect_true(res$n_unique_chars[[1]] == 2)
  expect_true(res$n_upper[[2]] == 1)
  expect_true(res$n_letters[[1]] == 8)
  expect_equal(
    res$prop_caps,
    res$n_upper / res$n_letters
  )
  expect_true(res$n_letters[[2]] == 29)
  expect_true(res$avg_word_length[[3]] == 6.2)
  expect_true(res$min_word_length[[2]] == 3)
  expect_equal(res$entropy[[2]], 3.6558333)
})

test_that("In the extract_keyword_features, the integration with the package Matrix is not working correctly", {
  keywords_long_list <- rep('a', 100001)
  keywords <- c('aa aa bb', 'aa bb', '123 aa', '123 bb', 'c' , NA)

  expect_warning(
    extract_keyword_features(keywords_long_list, use_matrix = FALSE)
  )

  expect_s4_class(
    extract_keyword_features(keywords, convert_to_df = FALSE),
    'Matrix'
  )
})


test_that("Function extract_keyword_features returns incorrect dimensions", {
  keywords <- c('aa aa bb', 'aa bb', '123 aa', '123 bb', 'c' , NA)
  #6 different descriptions

  expect_equal(
    colnames(extract_keyword_features(c('aa', 'aa'))), 'aa'
  )

  expect_equal(dim(extract_keyword_features(keywords)), c(6, 3))

  expect_equal(dim(extract_keyword_features(keywords, min_freq = 2)), c(6, 2))
})

test_that("Incorrect columns created in the function aggregate_psd2_keyword_features", {
  test_data <- data.frame(
    application_id = c(1, 1, 1, 2, 2),
    description = c('Test33', NA, '', 'test 123 a', 'ab, a')
  )

  expect_equal({
    colnames(
      aggregate_psd2_keyword_features(
        data = test_data,
        id_col = "application_id",
        description_col = "description",
        min_freq = 1
      )
    )
  }, c('application_id', 'a_count', 'ab_count', 'test_count'))
})

test_that("Incorrect number of ketwords counted in the function aggregate_psd2_keyword_features", {
  test_data <- data.frame(
    application_id = c(1, 1, 1, 2, 2),
    description = c('Test33', NA, '', 'test 123 a', 'ab, a')
  )

  expect_equal({
    as.vector(unlist(
      aggregate_psd2_keyword_features(
        data = test_data,
        id_col = "application_id",
        description_col = "description",
        min_freq = 1
      )[1, 2:4]
    ))
  }, c(0, 0, 1))

  expect_equal({
    as.vector(unlist(
      aggregate_psd2_keyword_features(
        data = test_data,
        id_col = "application_id",
        description_col = "description",
        min_freq = 1
      )[2, 2:4]
    ))
  }, c(2, 1, 1))
})

test_that("Dropped application ids in the function aggregate_psd2_keyword_features", {
  data("featForge_transactions")

  n_unique <- length(unique(featForge_transactions$application_id))

  expect_equal({
    nrow(
      aggregate_psd2_keyword_features(
        data = featForge_transactions,
        id_col = "application_id",
        description_col = "description",
        amount_col = "amount",
        time_col = "transaction_date",
        scrape_date_col = "scrape_date",
        observation_window_start_col = "obs_start",
        period = c(1, 1),
        ops = list(amount = sum),
        min_freq = 1,
        use_matrix = TRUE,
        convert_to_df = TRUE
      )
    )
  }, n_unique)
})
