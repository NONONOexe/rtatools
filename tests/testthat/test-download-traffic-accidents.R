destdir <- "data"
if (!fs::dir_exists(destdir)) fs::dir_create(destdir)

expect_succeed_download <- function(filename, download) {
  expect_path <- fs::path(destdir, filename)
  if (fs::file_exists(expect_path)) {
    return(succeed())
  }

  destfile <- download(destdir)
  expect_equal(expect_path, destfile)
  expect_true(fs::file_exists(destfile))
}

test_that("download honhyo, traffic accident main data", {
  filename <- "traffic-accident-2021-main.csv"
  expect_succeed_download(filename, download_traffic_accidents_main)
})

test_that("download hojuhyo, traffic accident sub data", {
  filename <- "traffic-accident-2021-sub.csv"
  expect_succeed_download(filename, download_traffic_accidents_sub)
})

test_that("download kosokuhyo, traffic accident highway data", {
  filename <- "traffic-accident-2021-highway.csv"
  expect_succeed_download(filename, download_traffic_accidents_highway)
})

test_that("download codebook, traffic accident code data", {
  filename <- "traffic-accident-2021-code.pdf"
  expect_succeed_download(filename, download_traffic_accidents_code)
})
