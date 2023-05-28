destdir <- "data"
if (!fs::dir_exists(destdir)) fs::dir_create(destdir)

expect_succeed_download <- function(filename, download) {
  expect_path <- fs::path(destdir, filename)
  if (fs::file_exists(expect_path)) {
    message("using cache data ‘", expect_path, "’")
    return(succeed())
  }

  message("downloading data ‘", expect_path, "’")
  destfile <- download(destdir)
  expect_equal(expect_path, destfile)
  expect_true(fs::file_exists(destfile))
}

test_that("download honhyo, traffic accident main data", {
  filename <- "traffic-accident-2021-main.csv"
  expect_succeed_download(filename, download_traffic_accidents_main)
  expect_no_error(read_traffic_accidents_main(fs::path(destdir, filename)))
})

test_that("download hojuhyo, traffic accident sub data", {
  filename <- "traffic-accident-2021-sub.csv"
  expect_succeed_download(filename, download_traffic_accidents_sub)
  expect_no_error(read_traffic_accidents_sub(fs::path(destdir, filename)))
})

test_that("download kosokuhyo, traffic accident highway data", {
  filename <- "traffic-accident-2021-highway.csv"
  expect_succeed_download(filename, download_traffic_accidents_highway)
  expect_no_error(read_traffic_accidents_highway(fs::path(destdir, filename)))
})

test_that("download codebook, traffic accident code data", {
  filename <- "traffic-accident-2021-code.pdf"
  expect_succeed_download(filename, download_traffic_accidents_code)
  expect_no_error(read_traffic_accidents_code(fs::path(destdir, filename)))
})
