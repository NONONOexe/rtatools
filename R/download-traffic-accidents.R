#' Download traffic accident data
#'
#' Download a file of traffic accident data from the National Police Agency's
#' Web page (https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html).
#' The type of traffic accident data include main (honhyo) , sub (hojuhyo),
#' highway (kosokuhyo), or code (codebook).
#' The downloaded file will be saved in the specified directory
#' with the naming format `"traffic-accident-{year}-{type}.{ext}"`.
#' `ext` is the file extension: `"pdf"` if `type` is `"code"`,
#' `"csv"` other `type`s.
#'
#' @param destdir A directory where downloaded file is to saved.
#' @param year Year the traffic accident data was recorded.
#' @return A path string of the downloaded file
#' @export
#' @examples
#' \dontrun{
#' # Download data sets.
#' download_traffic_accidents_main("data")
#' download_traffic_accidents_sub("data")
#' download_traffic_accidents_highway("data")
#' download_traffic_accidents_code("data")
#' }
download_traffic_accidents_main <- function(destdir = NULL, year = 2021) {
  download_traffic_accidents("main", destdir, year)
}

#' @rdname download_traffic_accidents_main
#' @export
download_traffic_accidents_sub <- function(destdir = NULL, year = 2021) {
  download_traffic_accidents("sub", destdir, year)
}

#' @rdname download_traffic_accidents_main
#' @export
download_traffic_accidents_highway <- function(destdir = NULL, year = 2021) {
  download_traffic_accidents("highway", destdir, year)
}

#' @rdname download_traffic_accidents_main
#' @export
download_traffic_accidents_code <- function(destdir = NULL, year = 2021) {
  download_traffic_accidents("code", destdir, year)
}

download_traffic_accidents <- function(type = c("main", "sub", "highway", "code"), destdir = NULL, year = 2021) {
  type <- match.arg(type)
  name <- c(
    main = "honhyo",
    sub = "hojuhyo",
    highway = "kosokuhyo",
    code = "codebook"
  )[type]
  ext <- ifelse(type == "code", "pdf", "csv")

  if (is.null(destdir)) destdir <- tempdir()

  page <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"
  url <- str_glue("{page}/{year}/{name}_{year}.{ext}")
  filename <- str_glue("traffic-accident-{year}-{type}.{ext}")
  destfile <- path(destdir, filename)

  download.file(url, destfile)

  return(invisible(destfile))
}
