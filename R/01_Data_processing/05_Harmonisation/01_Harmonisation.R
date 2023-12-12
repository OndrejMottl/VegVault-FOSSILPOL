#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow
#
#                 Harmonise the pollen taxa
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2023
#
#----------------------------------------------------------#

# Prepare all harmonisation tables and harmonise the raw counts


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# set the current environment
current_env <- rlang::current_env()

RUtilpol::output_heading(
  msg = "Preparation of harmonisation tables and harmonisation of pollen taxa"
)


#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

data_with_chronologies <-
  RUtilpol::get_latest_file(
    file_name = "data_with_chronologies",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Data_with_chronologies"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "data_with_chronologies",
  env = current_env
)

taxa_reference_table <-
  RUtilpol::get_latest_file(
    file_name = "taxa_reference_table",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Input",
      "/Harmonisation_tables"
    )
  )


#----------------------------------------------------------#
# 3. Prepare harmonisation tables -----
#----------------------------------------------------------#

# get all harmonisation tables
harmonisation_tables <-
  RFossilpol::harm_get_all_tables(
    data_source = data_with_chronologies,
    dir = data_storage_path # [config_criteria]
  )

taxa_reference_table_clean <-
  taxa_reference_table %>%
  dplyr::distinct(
    neotoma_names,
    .keep_all = TRUE
  ) %>%
  dplyr::mutate(
    neotoma_names_clean = janitor::make_clean_names(
      neotoma_names,
      case = "title",
      use_make_names = FALSE,
      allow_dupes = TRUE,
      sep_out = " "
    )
  ) %>%
  dplyr::select(
    taxon_name, neotoma_names_clean
  )

if (
  harmonisation_tables %>%
    tidyr::unnest(harm_table) %>%
    dplyr::select(
      dplyr::any_of("level_1")
    ) %>%
    purrr::pluck(1) %>%
    is.na() %>%
    any()
) {
  harmonisation_tables_filled <-
    harmonisation_tables %>%
    dplyr::mutate(
      harm_table = purrr::map(
        .x = harm_table,
        .f = ~ taxa_reference_table_clean %>%
          dplyr::left_join(
            .x, .,
            by = "taxon_name"
          ) %>%
          dplyr::select(
            !dplyr::any_of("level_1")
          )
      )
    )

  purrr::map2(
    .x = harmonisation_tables_filled$harmonisation_region,
    .y = harmonisation_tables_filled$harm_table,
    .f = ~ RUtilpol::save_latest_file(
      object_to_save = .y,
      file_name = .x,
      dir = here::here(
        "Data/Input/Harmonisation_tables/"
      ),
      prefered_format = "csv"
    )
  )

  # get all harmonisation tables
  harmonisation_tables <-
    RFossilpol::harm_get_all_tables(
      data_source = data_with_chronologies,
      dir = data_storage_path # [config_criteria]
    )
}

#----------------------------------------------------------#
# 4. Harmonise data -----
#----------------------------------------------------------#

data_harmonised <-
  RFossilpol::harmonise_all_regions(
    data_source = data_with_chronologies,
    harmonisation_tables = harmonisation_tables,
    original_name = "taxon_name",
    harm_level = "neotoma_names_clean", # [USER] Change the levels if needed
    exclude_taxa = "delete",
    pollen_grain_test = TRUE # [USER] Turn FALSE to hide progress
  )

#----------------------------------------------------------#
# 5. Save the data  -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_harmonised,
  dir = paste0(
    data_storage_path, # [config_criteria]
    "/Data/Processed/Data_harmonised"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)
