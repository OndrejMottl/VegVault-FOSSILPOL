#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow
#
#             Produce the final data assembly
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2023
#
#----------------------------------------------------------#

# Optionaly selecte the variables in the filtered data and save it


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

# Size limits for the age-uncertainty parts. The parts are committed to
#   this repository and VegVault reads them from raw GitHub URLs. GitHub
#   warns about files larger than 50 MiB and blocks files larger than
#   100 MiB (GitHub Docs, "About large files on GitHub").
#   - `max_part_bytes`: the estimated qs2 size at which a new part starts,
#     set to the warning size. The estimate sums per-row serialisations,
#     so a saved file can be slightly larger or smaller than it
#   - `github_max_file_bytes`: the hard limit every saved part is checked
#     against
max_part_bytes <-
  50 * 2^20

github_max_file_bytes <-
  100 * 2^20


#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_filtered <-
  RUtilpol::get_latest_file(
    file_name = "data_filtered",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Data_filtered/"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "data_filtered",
  env = current_env
)


#----------------------------------------------------------#
# 3. Save whole assembly  -----
#----------------------------------------------------------#

RFossilpol::proc_save_assembly(
  data_source = data_filtered,
  user_sel_variables = c("long", "lat"), # [USER] Here can user variables,
  # which have to be present in the  final data assembly (other can be selected
  # interactively, if `select_final_variables` is TRUE)
  select_final_variables = select_final_variables, # [config_criteria]
  dir = data_storage_path # [config_criteria]
)

#----------------------------------------------------------#
# 4. Save individual parts  -----
#----------------------------------------------------------#

# as the whole assembly is very large. It is a good idea to
# save individual parts of it as `qs2` files (with the `.qs` extension)

data_filtered %>%
  dplyr::select(
    -c(age_uncertainty, raw_counts)
  ) %>%
  RUtilpol::save_latest_file(
    object_to_save = .,
    file_name = "data_assembly_light",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Outputs/Data/"
    ),
    prefered_format = "qs"
  )

data_age_uncertainty <-
  data_filtered %>%
  dplyr::select(
    dataset_id, age_uncertainty
  )

# Split the age uncertainty into consecutive parts (A, B, C, ...), each
#   below `max_part_bytes`, keeping the row order of `data_filtered`
list_age_uncertainty_parts <-
  build_size_limited_parts(
    data_source = data_age_uncertainty,
    max_part_bytes = max_part_bytes
  )

path_outputs_data <-
  stringr::str_c(
    data_storage_path, # [config_criteria]
    "/Outputs/Data/"
  )

purrr::iwalk(
  .x = list_age_uncertainty_parts,
  .f = ~ RUtilpol::save_latest_file(
    object_to_save = .x,
    file_name = stringr::str_c("data_age_uncertainty_", .y),
    dir = path_outputs_data,
    prefered_format = "qs"
  )
)


#--------------------------------------------------#
## 4.1. Check the saved age-uncertainty parts -----
#--------------------------------------------------#

# Read back every age-uncertainty part in the output folder, as VegVault
#   does. `RUtilpol::save_latest_file()` removes an older file only when it
#   has the same date, so a part left from an earlier run (for example a
#   `_C` after a run that needs only two parts) is read as well and makes
#   the check fail.
vec_age_uncertainty_part_names <-
  base::list.files(
    path = path_outputs_data,
    pattern = "^data_age_uncertainty_[A-Z]_"
  ) |>
  RUtilpol::get_clean_name() |>
  base::unique() |>
  base::sort()

vec_age_uncertainty_files <-
  purrr::map_chr(
    .x = vec_age_uncertainty_part_names,
    .f = ~ RUtilpol::get_latest_file_name(
      file_name = .x,
      dir = path_outputs_data,
      verbose = FALSE
    )
  )

vec_age_uncertainty_file_bytes <-
  base::file.size(
    stringr::str_c(path_outputs_data, vec_age_uncertainty_files)
  )

vec_age_uncertainty_files_too_large <-
  vec_age_uncertainty_files[
    vec_age_uncertainty_file_bytes > github_max_file_bytes
  ]

if (
  base::length(vec_age_uncertainty_files_too_large) > 0L
) {
  cli::cli_abort(
    c(
      "Age-uncertainty parts larger than the GitHub file limit:
      {.file {vec_age_uncertainty_files_too_large}}.",
      "i" = "Lower {.code max_part_bytes} in section 1 and rerun."
    )
  )
}

list_age_uncertainty_parts_saved <-
  purrr::map(
    .x = vec_age_uncertainty_files,
    .f = ~ qs2::qs_read(
      file = stringr::str_c(path_outputs_data, .x)
    )
  )

validate_export_parts(
  list_parts = list_age_uncertainty_parts_saved,
  data_reference = data_age_uncertainty,
  id_column = "dataset_id"
)
