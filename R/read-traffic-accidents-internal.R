#' Read internal data of traffic accidents
#'
#' Read a file of traffic accident data for a specified path.
#'
#' @param file A file path of data to read
#' @return Data read in `tibble` format
#' @export
#' @examples
#' \dontrun{
#' data <- read_traffic_accidents_internal("2023.xlsx")
#' }
read_traffic_accidents_internal <- function(file) {
  data_raw <- file |>
    read.xlsx(colNames = FALSE) |>
    map_df(as.character) |>
    map_df(str_trim)
  item_names <- data_raw[1,] |>
    as.vector() |>
    translate("internal", "item_name")
  data_raw <- set_names(data_raw[-1,], item_names)
  data <- list(
    traffic_accidents = data_raw |> extract_traffic_accidents_internal(),
    parties           = data_raw |> extract_parties_internal()
  )
  
  return(data)
}

.traffic_accidents_internal_cols <- c(
  "accident_id", "occurrence_date", "day_of_week", "day_night_type", 
  "occurrence_hour", "police_office", "occurrence_place",
  "latitude", "longitude", "weather", "road_surface", "road_type",
  "road_shape", "road_alignment", "carriageway_width",
  "traffic_signal", "injury_pattern", "fatality", "severe_injury",
  "slight_injury", "impact_type", "collision_position",
  "special_category_1", "special_category_2", "special_category_3"
)

.parties_internal_cols <- c(
  "accident_id", "car_id", "passenger_id", "party_rank",
  "violation_type", "violation_detail", "cause_road", "cause_car",
  "cause_human", "action_type", "move_direction", "car_light_state",
  "party_type", "party_subtype", "party_subsubtype", "car_tire",
  "use_type", "use_detail", "injured_part", "injury_level", "seat_belt",
  "helmet", "air_bag", "side_air_bag", "alcohol_intake", "cell_phone",
  "car_nav_system", "critical_speed", "party_gender", "party_age",
  "home_prefecture", "home_address", "home_distance", "party_job", "purpose"
)

extract_traffic_accidents_internal <- function(data_raw) {
  traffic_accidents <- data_raw |>
    distinct(accident_id, .keep_all =TRUE) |>
    mutate(
      occurrence_date = .data$occurrence_date |> as_date(),
      occurrence_hour = .data$occurrence_hour |> as.integer(),
      latitude        = .data$latitude |>
        str_replace("^([0-9]{3})([0-9]{8})$", "\\1.\\2") |>
        as.numeric() |>
        convert_deg(),
      longitude       = .data$longitude |>
        str_replace("^([0-9]{3})([0-9]{8})$", "\\1.\\2") |>
        as.numeric() |>
        convert_deg(),
      fatality        = .data$fatality      |> as.integer(),
      severe_injury   = .data$severe_injury |> as.integer(),
      slight_injury   = .data$slight_injury |> as.integer()
    ) |>
    select(any_of(.traffic_accidents_internal_cols)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  return(traffic_accidents)
}

extract_parties_internal <- function(data_raw) {
  parties <- data_raw |>
    mutate(party_age = .data$party_age |> as.integer()) |>
    select(any_of(.parties_internal_cols))
  
  return(parties)
}
