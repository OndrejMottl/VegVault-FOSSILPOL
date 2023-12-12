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
# save individual parts of it using `qs - archive`

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
    prefered_format = "qs",
    preset = "archive",
  )

data_age_uncertainty <-
  data_filtered %>%
  dplyr::select(
    dataset_id, age_uncertainty
  )

RUtilpol::save_latest_file(
  object_to_save = data_age_uncertainty %>%
    dplyr::slice(1:700),
  file_name = "data_age_uncertainty_A",
  dir = paste0(
    data_storage_path, # [config_criteria]
    "/Outputs/Data/"
  ),
  prefered_format = "qs",
  preset = "archive",
)

RUtilpol::save_latest_file(
  object_to_save = data_age_uncertainty %>%
    dplyr::slice(701:10e3),
  file_name = "data_age_uncertainty_B",
  dir = paste0(
    data_storage_path, # [config_criteria]
    "/Outputs/Data/"
  ),
  prefered_format = "qs",
  preset = "archive",
)
