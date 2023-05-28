translate <- function(names, category = NULL, subcategory = NULL) {
  dict <- dictionary
  if (!is.null(category)) {
    if (!category %in% dictionary$categories) {
      stop("The specified category is not supported: ", category)
    }
    dict <- dict |> filter(.data$categories == category)

    if (!is.null(subcategory)) {
      if (!subcategory %in% dictionary$subcategories) {
        stop("The specified subcategory is not supported: ", category)
      }
      dict <- dict |> filter(.data$subcategories == subcategory)
    }
  }
  conv <- set_names(dict$english, dict$japanese)
  translated <- unname(conv[names])

  return(translated)
}

translate_file_name <- function(file_names) {
  translate(file_names, category = "file_name")
}

translate_item_name <- function(item_names) {
  jp_party <- dictionary$japanese[dictionary$english == "party"]
  translated <- item_names |>
    stri_trans_general("fullwidth-halfwidth") |>
    str_remove_all(str_c("\\(", jp_party, "(A|B)\\)")) |>
    stri_trans_general("halfwidth-fullwidth") |>
    translate(category = "item_name")
  suffix <- item_names |>
    stri_trans_general("fullwidth-halfwidth") |>
    str_extract(str_c("\\(", jp_party, "(A|B)\\)")) |>
    str_replace_na("") |>
    str_replace_all(set_names(
      str_c("__", c("A", "B")),
      str_c("\\(", jp_party, c("A", "B"), "\\)")))
  translated <- str_c(translated, suffix)

  return(translated)
}

translate_code_column_name <- function(column_names) {
  translate(column_names, category = "column_name")
}
