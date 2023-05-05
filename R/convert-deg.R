#' Convert form of latitude and longitude from DMS to DEG
#'
#' Converts latitude and longitude expressed in
#' DMS (Degree, Minute, Second) format to decimal notation,
#' DEG (Degree) format.
#'
#' @param dms Latitude or longitude in DMS format.
#' @return Latitude or longitude converted to DEG format.
#' @export
#' @examples
#' convert_deg("40.3253820") # 40.54828
#' convert_deg("140.3554682") # 140.5985
convert_deg <- function(dms) {
  dms_num <- as.numeric(dms)
  dms_str <- number(dms_num, accuracy = .00000001, digits = 8)

  d <- dms_str |>
    str_replace("^(.+)\\..{8}$", "\\1") |>
    str_replace_na("0")
  m <- dms_str |>
    str_replace("^.+\\.(.{2}).{6}$", "\\1") |>
    str_replace_na("0")
  s <- dms_str |>
    str_replace("^.+\\..{2}(.{2})(.{4})$", "\\1.\\2") |>
    str_replace_na("0")

  deg <- if_else(is.na(dms), dms_num, dms2deg(d, m, s))

  return(deg)
}
