bin_taxonomic_data <- function(data_source, var_name, n_taxa) {
  data_source %>%
    dplyr::distinct({{ var_name }}) %>%
    dplyr::mutate(
      taxa_id = dplyr::row_number()
    ) %>%
    dplyr::mutate(
      taxa_bin := ceiling(taxa_id / n_taxa_bulk) * n_taxa_bulk
    ) %>%
    dplyr::group_by(taxa_bin) %>%
    tidyr::nest() %>%
    return()
}
