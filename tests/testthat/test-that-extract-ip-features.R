
test_that("extract_ip_features error on extracting v4 features", {
  normal_ip_v4 <- extract_ip_features('192.168.0.1')

  expect_equal(normal_ip_v4$ip_version[[1]], 'IPv4')
  expect_equal(normal_ip_v4$ip_v4_octet_1[[1]], 192)
  expect_equal(normal_ip_v4$ ip_v4_numeric_vector[[1]], 3232235521)
  expect_equal(normal_ip_v4$ip_version[[1]], 'IPv4')
  expect_equal(normal_ip_v4$ip_version[[1]], 'IPv4')

  expect_equal(extract_ip_features('192.168.001.001')[['ip_leading_zero_count']],
               2)
})

test_that("extract_ip_features error on extracting v6 features", {
  normal_ip_v6 <- extract_ip_features('2607:f8b0:4005:0802:0000:0000:0000:200e')

  expect_equal(normal_ip_v6$ip_version[[1]], 'IPv6')
  expect_equal(normal_ip_v6$ip_v6_numeric_approx_vector[[1]], 5.055205e+37,
               tolerance = 0.001)
})

test_that("extract_ip_features error on extracting dealing with non-IP values", {
  non_ips <- c('test', '', '25.25.25', '1234::abcde', NA)

  expect_warning(extract_ip_features(non_ips))

  suppressWarnings({
    res <- extract_ip_features(non_ips)
  })

  expect_true(all(is.na(res)))

  expect_error(extract_ip_features(non_ips, error_on_invalid = TRUE))
})

test_that("extract_ip_features incorrect output dimensions", {

  data("featForge_sample_data")

  suppressWarnings({
    res <- extract_ip_features(featForge_sample_data$ip)
  })

  expect_equal(nrow(res), nrow(featForge_sample_data))

  #Ensure that the row order hasn't been changed
  ipv4 <- which(res$ip_version == 'IPv4' &
                  !is.na(res$ip_version))
  res2 <- extract_ip_features(featForge_sample_data$ip[ipv4])

  expect_equal(res$ip_v4_numeric_vector[ipv4], res2$ip_v4_numeric_vector)
})


