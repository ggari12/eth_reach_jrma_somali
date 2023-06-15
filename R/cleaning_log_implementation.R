# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# Read data and checking log 

df_cleaning_log <- read_csv("inputs/combined_checks_eth_jrma_somali.csv", show_col_types = FALSE) |> 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA,
         index = NA,
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, relevant, issue)

# raw data
loc_data <- "inputs/ETH2303_JRMA_Somali_data.xlsx"

cols_to_escape <- c("start", "end", "today", "startTime",	"endTime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_os$|_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# tool
loc_tool <- "inputs/ETH2303_JRMA_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey") |> 
  mutate(name=str_replace(name, "_os$", "_other"))
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::English`)


# main dataset ------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log 

df_cleaned_data <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                               input_df_survey = df_survey,
                                               input_df_choices = df_choices,
                                               input_df_cleaning_log = df_cleaning_log_main)

# Add composite indicators at this stage ----------------------------------



# deletion log ------------------------------------------------------------

df_deletion_log <- df_cleaning_log |> 
  filter(type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup()

# write final datasets out -----------------------------------------------

list_of_clean_datasets <- list("Raw_main" = df_raw_data,
                               "cleaning_log" = df_cleaning_log,
                               "deletion_log" = df_deletion_log,
                               "cleaned_data" = df_cleaned_data
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "__clean_data_eth_jrma_somali.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")


# openxlsx::write.xlsx(x = list_of_clean_datasets,
#                      file = paste0("inputs/clean_data_eth_jrma_somali.xlsx"), 
#                      overwrite = TRUE, keepNA = TRUE, na.string = "NA")
