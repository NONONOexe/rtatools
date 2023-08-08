translate <- function(names, file, target) {
  if (!file %in% dictionary$file) {
    stop("The specified file is not supported, file: ", file)
  }
  if (!target %in% dictionary$target) {
    stop(
      "The specified target is not supported, file: ",
      file, ", target: ", target
    )
  }
  target_dictionary <- dictionary |>
    filter(.data$file == file) |>
    filter(.data$target == target)
  pairs_en_jp <- set_names(
    target_dictionary$english,
    target_dictionary$japanese
  )
  names_fixed <- names |> stri_trans_general("halfwidth-fullwidth")
  translated <- pairs_en_jp[names_fixed] |>
    unname() |>
    replace_na("unsupported") |>
    make.unique()

  return(translated)
}
