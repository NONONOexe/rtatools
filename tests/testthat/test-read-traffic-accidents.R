test_that("read honhyo, traffic accident main data", {
  path <- "data/traffic-accident-2021-main.csv"
  expect_no_error(read_traffic_accidents_main(path))
})

test_that("read hojuhyo, traffic accident sub data", {
  path <- "data/traffic-accident-2021-sub.csv"
  expect_no_error(read_traffic_accidents_sub(path))
})

test_that("read kosokuhyo, traffic accident highway data", {
  path <- "data/traffic-accident-2021-highway.csv"
  expect_no_error(read_traffic_accidents_highway(path))
})

test_that("read codebook, traffic accident code data", {
  path <- "data/traffic-accident-2021-code.pdf"
  expect_no_error(read_traffic_accidents_code(path))
})
