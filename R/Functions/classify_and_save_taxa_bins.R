classify_and_save_taxa_bins <- function(data_source, sel_var_name, dir) {
  purrr::walk2(
    .x = data_source$taxa_bin,
    .y = data_source$data,
    .f = ~ {
      sel_bin <- as.character(.x)

      taxospace_res <-
        .y %>%
        purrr::chuck(sel_var_name) %>%
        taxospace::get_classification_buk(
          verbose = TRUE
        )

      RUtilpol::save_latest_file(
        object_to_save = taxospace_res,
        file_name = sel_bin,
        dir = dir
      )
    }
  )
}
