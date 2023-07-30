#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow
#
#                 Classify the pollen taxa
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2023
#
#----------------------------------------------------------#

# Get taxonomic names for all taxa from GBIF database


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)


#----------------------------------------------------------#
# 2. Automatic to GBIF taxa -----
#----------------------------------------------------------#

# Get the taxa refference table and add classification for all taxa
taxa_reference_table <-
  RUtilpol::get_latest_file(
    "taxa_reference_table",
    dir = here::here(
      "Data/Input/Harmonisation_tables"
    )
  )

# set number of taxa to bin data by
n_taxa_bulk <- 50

# nest data
taxa_table_raw_nested <-
  bin_taxonomic_data(
    taxa_reference_table,
    neotoma_names,
    n_taxa_bulk
  )

if (
  here::here(
    "Data/Temp/classification_neotoma"
  ) %>%
    list.files() %>%
    length() == 0
) {
  # get classification for all taxa in a bin and save
  classify_and_save_taxa_bins(
    taxa_table_raw_nested,
    "neotoma_names",
    dir = here::here(
      "Data/Temp/classification_neotoma"
    )
  )
}

# load data
data_reference_table_class <-
  get_all_data_as_df(
    here::here(
      "Data/Temp/classification_neotoma"
    )
  )

# merge data into harmonisation table
data_taxa_harmonised <-
  dplyr::left_join(
    taxa_reference_table,
    data_reference_table_class,
    by = dplyr::join_by("neotoma_names" == "sel_name")
  ) %>%
  dplyr::mutate(
    taxon_harmonised = purrr::map_chr(
      .x = classification,
      .f = ~ {
        if (
          is.null(.x)
        ) {
          return(NA_character_)
        }

        .x %>%
          dplyr::slice_tail(n = 1) %>%
          purrr::chuck("name") %>%
          return()
      }
    )
  ) %>%
  dplyr::select(
    neotoma_names,
    taxon_name,
    taxon_harmonised,
    classification
  )


#----------------------------------------------------------#
# 3. Use taxon_name for missing data -----
#----------------------------------------------------------#

# get the missin taxa
data_taxa_harmonised_missing <-
  data_taxa_harmonised %>%
  dplyr::filter(
    is.na(taxon_harmonised)
  ) %>%
  dplyr::select(-c(taxon_harmonised, classification))

# nest missing data
data_taxa_harmonised_missing_nested <-
  bin_taxonomic_data(
    data_taxa_harmonised_missing,
    taxon_name,
    n_taxa_bulk
  )

if (
  here::here(
    "Data/Temp/classification_taxon_name"
  ) %>%
    list.files() %>%
    length() == 0
) {
  # get classification for all missing taxa in a bin and save
  classify_and_save_taxa_bins(
    data_taxa_harmonised_missing_nested,
    "taxon_name",
    dir = here::here(
      "Data/Temp/classification_taxon_name"
    )
  )
}

# load data
data_taxa_missing_class <-
  get_all_data_as_df(
    here::here(
      "Data/Temp/classification_taxon_name"
    )
  )

data_taxa_missing_harmonised <-
  dplyr::left_join(
    data_taxa_harmonised_missing,
    data_taxa_missing_class,
    by = dplyr::join_by("taxon_name" == "sel_name")
  ) %>%
  dplyr::mutate(
    taxon_harmonised = purrr::map_chr(
      .x = classification,
      .f = ~ {
        if (
          is.null(.x)
        ) {
          return(NA_character_)
        }

        .x %>%
          dplyr::slice_tail(n = 1) %>%
          purrr::chuck("name") %>%
          return()
      }
    )
  ) %>%
  dplyr::select(
    neotoma_names,
    taxon_name,
    taxon_harmonised,
    classification
  )

data_taxa_harmonised_merged <-
  dplyr::left_join(
    data_taxa_harmonised,
    data_taxa_missing_harmonised,
    by = dplyr::join_by(neotoma_names, taxon_name),
    suffix = c("_neotoma", "_taxon_name")
  ) %>%
  dplyr::mutate(
    taxon_harmonised = ifelse(
      is.na(taxon_harmonised_neotoma),
      taxon_harmonised_taxon_name,
      taxon_harmonised_neotoma
    ),
    classification = ifelse(
      is.null(classification_neotoma),
      classification_taxon_name,
      classification_neotoma
    )
  ) %>%
  dplyr::select(
    neotoma_names,
    taxon_name,
    taxon_harmonised,
    classification
  )


#----------------------------------------------------------#
# 4. Manually edit names for missing data -----
#----------------------------------------------------------#

# get data for manual edit
data_taxa_manual_edit <-
  data_taxa_harmonised_merged %>%
  dplyr::filter(
    is.na(taxon_harmonised)
  ) %>%
  dplyr::select(-c(taxon_harmonised, classification)) %>%
  dplyr::mutate(
    taxon_name_spaces = stringr::str_replace(
      taxon_name, "_type", ""
    ) %>%
      stringr::str_replace_all(
        ., "_", " "
      ) %>%
      stringr::str_replace(
        ., "cf ", ""
      )
  )

# nest missing data
data_taxa_manual_edit_nested <-
  bin_taxonomic_data(
    data_taxa_manual_edit,
    taxon_name_spaces,
    n_taxa_bulk
  )

if (
  here::here(
    "Data/Temp/classification_maual_edit"
  ) %>%
    list.files() %>%
    length() == 0
) {
  # get classification for all missing taxa in a bin and save
  classify_and_save_taxa_bins(
    data_taxa_manual_edit_nested,
    "taxon_name_spaces",
    dir = here::here(
      "Data/Temp/classification_maual_edit"
    )
  )
}

# load data
data_taxa_manual_edit_class <-
  get_all_data_as_df(
    here::here(
      "Data/Temp/classification_maual_edit"
    )
  ) %>%
  tibble::as_tibble()

# save data for manual search
data_taxa_manual_edit_class %>%
  dplyr::filter(is.na(id)) %>%
  dplyr::distinct(sel_name) %>%
  dplyr::mutate(
    manual_name = NA_character_
  ) %>%
  RUtilpol::save_latest_file(
    object_to_save = .,
    file_name = "missing_taxa",
    dir = here::here(
      "Data/Input/Harmonisation_tables/"
    ),
    prefered_format = "csv"
  )


#----------------------------------------------------------#
# 5. Look up names for missing data -----
#----------------------------------------------------------#

# MANUAL edit of the file here!


#----------------------------------------------------------#
# 6. Merge and save data -----
#----------------------------------------------------------#

# load the edited file
data_taxa_manual_search <-
  RUtilpol::get_latest_file(
    file_name = "missing_taxa",
    dir = here::here(
      "Data/Input/Harmonisation_tables/"
    )
  )

data_taxa_manual_search_class <-
  dplyr::left_join(
    data_taxa_manual_search,
    taxospace::get_classification_buk(
      taxa_vec = data_taxa_manual_search$manual_name
    ),
    by = dplyr::join_by(manual_name == sel_name)
  ) %>%
  dplyr::select(-manual_name)

data_taxa_manual_harmonised <-
  data_taxa_manual_edit_class %>%
  dplyr::filter(
    !is.na(id)
  ) %>%
  dplyr::bind_rows(data_taxa_manual_search_class) %>%
  dplyr::full_join(
    data_taxa_manual_edit,
    .,
    by = dplyr::join_by(taxon_name_spaces == sel_name),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    taxon_harmonised = purrr::map_chr(
      .x = classification,
      .f = ~ {
        if (
          is.null(.x)
        ) {
          return(NA_character_)
        }

        .x %>%
          dplyr::slice_tail(n = 1) %>%
          purrr::chuck("name") %>%
          return()
      }
    )
  ) %>%
  dplyr::select(
    neotoma_names,
    taxon_name,
    taxon_harmonised,
    classification
  )

data_taxa_harmonised_full <-
  dplyr::left_join(
    data_taxa_harmonised_merged,
    data_taxa_manual_harmonised,
    by = dplyr::join_by(neotoma_names, taxon_name),
    suffix = c("_automatic", "_manual")
  ) %>%
  dplyr::mutate(
    taxon_harmonised = ifelse(
      is.na(taxon_harmonised_automatic),
      taxon_harmonised_manual,
      taxon_harmonised_automatic
    ),
    classification = ifelse(
      is.null(classification_automatic),
      classification_manual,
      classification_automatic
    )
  ) %>%
  dplyr::select(
    neotoma_names,
    taxon_name,
    taxon_harmonised,
    classification
  )

# save results
RUtilpol::save_latest_file(
  object_to_save = data_taxa_harmonised_full,
  file_name = "data_taxa_classification",
  dir = here::here(
    "Data/Input/Harmonisation_tables/"
  )
)
