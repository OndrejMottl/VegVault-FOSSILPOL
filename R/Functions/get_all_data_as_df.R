get_all_data_as_df <- function(dir) {
  list.files(dir) %>%
    purrr::map(
      .f = ~ RUtilpol::get_clean_name(.x)
    ) %>%
    unique() %>%
    rlang::set_names() %>%
    purrr::map(
      .f = ~ RUtilpol::get_latest_file(
        file_name = .x,
        dir = dir
      )
    ) %>%
    dplyr::bind_rows()
}