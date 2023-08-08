#' Read traffic accident data
#'
#' Read a file of traffic accident data for a specified path.
#' The type of traffic accident data include main (honhyo) , sub (hojuhyo),
#' highway (kosokuhyo), or code (codebook).
#'
#' @param file A file path of data to read
#' @return Data read in `tibble` format
#' @export
#' @examples
#' \dontrun{
#' # Read traffic accident main data (honhyo)
#' file <- download_traffic_accidents_main("data")
#' data <- read_traffic_accidents_main(file)
#' }
read_traffic_accidents_main <- function(file) {
  main_raw <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      name_repair    = \(names) translate(names, "main", "item_name"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    mutate(
      recording_year = file |>
        str_replace(".*traffic-accident-([0-9]{4}).*", "\\1") |>
        as.integer(),
      accident_id = .data$main_id
    )

  main <- list(
    traffic_accidents = main_raw |> extract_traffic_accidents(),
    parties           = main_raw |> extract_parties()
  )

  return(main)
}

#' @rdname read_traffic_accidents_main
#' @export
read_traffic_accidents_sub <- function(file) {
  parties <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      name_repair    = \(names) translate(names, "sub", "item_name"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    dplyr::mutate_all(replace_na, "") |>
    mutate(
      recording_year = file |>
        str_replace(".*traffic-accident-([0-9]{4}).*", "\\1") |>
        as.integer(),
      accident_id = .data$main_id,
      party_id = .data$sub_id,
      stop_sign = NA,
      stop_mark = NA,
      speed_limit = NA,
      party_age_group = NA
    ) |>
    select(all_of(.parties_cols))

  return(parties)
}

#' @rdname read_traffic_accidents_main
#' @export
read_traffic_accidents_highway <- function(file) {
  highways <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      name_repair    = \(names) translate(names, "highway", "item_name"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    mutate(
      recording_year = file |>
        str_replace(".*traffic-accident-([0-9]{4}).*", "\\1") |>
        as.integer(),
      accident_id = .data$main_id,
      tunnel_length = as.integer(.data$tunnel_length)
    ) |>
    select(all_of(.highways_cols))

  return(highways)
}

#' @rdname read_traffic_accidents_main
#' @export
read_traffic_accidents_code <- function(file) {
  code <-
    tibble(
      page          = read_pdf_data_without_page_number(file),
      text_left     = measure_text_left_margin(.data$page),
      item_name     = map_chr(.data$page, find_item_name),
      using_highway = map_lgl(.data$page, is_using_highway)
    ) |>
    fill("item_name", "using_highway") |>
    filter(.data$item_name %in% .codes_item_names &
      !(.data$item_name == "impact_type" & .data$using_highway)) |>
    mutate(
      header = pmap(
        list(.data$page, .data$text_left, .data$item_name),
        extract_header
      ),
      data = map2(.data$page, .data$header, extract_data)
    ) |>
    group_by(.data$item_name) |>
    summarise(data = list(reduce(.data$data, rbind))) |>
    unstack(data ~ item_name)

  return(code)
}

.traffic_accidents_cols <- c(
  "recording_year", "prefecture", "police_office", "accident_id",
  "occurence_time", "day_of_week", "holiday", "day_night_type", "city",
  "road", "track", "kilopost", "region_type", "zone_regulation",
  "road_verge", "center_divider", "weather", "road_surface", "road_shape",
  "road_alignment", "carriageway_width", "traffic_signal",
  "roundabout_diameter", "injury_pattern", "fatalities", "injuries",
  "impact_type", "collision_position", "geometry"
)

.parties_cols <- c(
  "recording_year", "prefecture", "police_office", "accident_id",
  "party_id", "party_type", "car_type", "use_type", "air_bag",
  "side_air_bag", "impact_point", "damage_level", "injure_level",
  "stop_sign", "stop_mark", "speed_limit", "party_age_group"
)

.highways_cols <- c(
  "recording_year", "prefecture", "police_office", "accident_id",
  "road_manager", "highway_type", "road_structure", "curve_radius",
  "longitudinal_slope", "tunnel", "car_count_group", "object",
  "temp_limit_type", "temp_limit", "tunnel_length"
)

.codes_item_names <- c(
  "prefecture", "police_office", "injury_pattern", "expressway",
  "track", "day_night_type", "weather", "region_type", "road_surface",
  "road_alignment", "roundabout_diameter", "traffic_signal",
  "stop_sign", "stop_mark", "carriageway_width", "road_alignment",
  "collision_position", "zone_regulation", "center_divider", "road_verge",
  "impact_type", "party_age_group", "car_type", "use_type", "speed_limit",
  "damage_level", "air_bag", "side_air_bag", "injure_level", "day_of_week",
  "holiday", "party_type", "road_manager", "highway_type",
  "road_structure", "curve_radius", "longitudinal_slope", "tunnel",
  "car_count_group", "object", "temp_limit_type", "temp_limit"
)

extract_traffic_accidents <- function(main_raw) {
  traffic_accidents <- main_raw |>
    mutate(
      occurence_time = make_datetime(
        year  = as.integer(.data$occurence_time_year),
        month = as.integer(.data$occurence_time_month),
        day   = as.integer(.data$occurence_time_day),
        hour  = as.integer(.data$occurence_time_hour),
        min   = as.integer(.data$occurence_time_min),
        tz    = "Asia/Tokyo"
      ),
      fatalities = as.integer(.data$fatalities),
      injuries = as.integer(.data$injuries),
      latitude = .data$latitude |>
        str_replace("^([0-9]{2})([0-9]{7})$", "\\1.\\2") |>
        convert_deg(),
      longitude = .data$longitude |>
        str_replace("^([0-9]{3})([0-9]{7})$", "\\1.\\2") |>
        convert_deg()
    ) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    select(all_of(.traffic_accidents_cols))

  return(traffic_accidents)
}

extract_parties <- function(main_raw) {
  parties <- main_raw |>
    pivot_longer(
      -c("recording_year", "prefecture", "police_office", "accident_id")
    ) |>
    filter(str_detect(.data$name, "[_]{2}")) |>
    separate_wider_delim("name",
      delim = "__", names = c("name", "party_id")
    ) |>
    filter(.data$name %in% .parties_cols) |>
    pivot_wider()

  return(parties)
}

read_pdf_data_without_page_number <- function(file) {
  data <- file |>
    pdf_data() |>
    map(\(page) page |> filter(.data$y != max(.data$y)))

  return(data)
}

measure_text_left_margin <- function(pages) {
  left_margin <- pages |>
    reduce(bind_rows) |>
    pull(.data$x) |>
    min()

  return(left_margin)
}

find_item_name <- function(page) {
  y_base <- page |>
    filter(.data$text == "\u9805\u76ee\u540d") |>
    pull(.data$y)

  if (length(y_base) == 0L) {
    return(NA)
  }

  item_name <- page |>
    filter(
      between(.data$y, y_base - 1, y_base + 1),
      .data$text != "\u9805\u76ee\u540d"
    ) |>
    arrange(.data$x) |>
    pull(.data$text) |>
    paste(collapse = " ") |>
    translate("code", "item_name")

  return(item_name)
}

is_using_highway <- function(page) {
  y_base <- page |>
    filter(.data$text == "\u9069\u7528") |>
    pull(.data$y)
  if (length(y_base) == 0L) {
    return(NA)
  }
  item_names <- page |>
    filter(
      between(.data$y, y_base - 1, y_base + 1),
      .data$text != "\u9069\u7528"
    ) |>
    pull(.data$text)
  if (length(item_names) == 0L) {
    return(NA)
  }

  return("\u9ad8\u901f\u7968" %in% item_names)
}

extract_header <- function(page, text_left, item_name) {
  y_range <- page |>
    filter(str_starts(.data$text, "\u30b3\u30fc\u30c9")) |>
    slice_head() |>
    transmute(top = .data$y, bottom = .data$y + .data$height)
  header <- page |>
    filter(between(.data$y + .data$height / 2, y_range$top, y_range$bottom)) |>
    mutate(center = .data$x + .data$width / 2) |>
    arrange(.data$center) |>
    transmute(
      name = .data$text |>
        translate("code", "column_name") |>
        str_replace_all(str_c("^", item_name, "$"), "code") |>
        str_replace_all(str_c("^", item_name, "_name$"), "name"),
      left = accumulate(.data$center,
        .init = text_left,
        \(left, center) 2 * center - left
      )[-n() - 1],
      right = 2 * .data$center - .data$left,
      top = .data$y,
      bottom = .data$top + .data$height
    )

  return(header)
}

extract_data <- function(page, header) {
  data <- page |>
    filter(max(header$bottom) < .data$y + .data$height / 2) |>
    cross_join(header) |>
    filter(between(.data$x + .data$width / 2, .data$left, .data$right)) |>
    arrange(.data$y) |>
    mutate(index = row_number()) |>
    nest(input = c("name", "y", "height")) |>
    mutate(row_number = accumulate(
      .x    = .data$input,
      .f    = determine_row_number,
      .init = list(num = 0L)
    )[-1] |> map_int(pluck, "num")) |>
    unnest("input") |>
    mutate(text = str_c(.data$text, ifelse(.data$space, "\u3000", ""))) |>
    group_by(.data$row_number, .data$name) |>
    summarise(value = reduce(.data$text, str_c), .groups = "drop") |>
    pivot_wider() |>
    select(-"row_number") |>
    mutate(across(everything(), \(x) replace_na(x, "")))

  if ("name" %in% names(data)) data <- relocate(data, "name")
  if ("code" %in% names(data)) data <- relocate(data, "code")

  return(data)
}

determine_row_number <- function(acc, cur) {
  if (acc$num == 0L || acc$bottom < cur$y + cur$height / 2) {
    acc <- list(
      num    = acc$num + 1L,
      top    = cur$y,
      bottom = Inf
    )
  }
  if (cur$name == "code") {
    acc$bottom <- 2 * cur$y + cur$height - acc$top
  }

  return(acc)
}
