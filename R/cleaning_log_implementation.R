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
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
loc_data <- "inputs/ETH2303_JRMA_Somali_data.xlsx"

cols_to_escape <- c("start", "end", "today", "startTime",	"endTime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_os$|_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(start = as_datetime(startTime),
         end = as_datetime(endTime)) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) |> 
  rename_with(~str_replace(string = .x, pattern = "_os$", replacement = "_other")) |> 
  rename_with(~str_replace(string = .x, pattern = "\\+", replacement = "_")) |> 
  rename_with(~str_replace(string = .x, pattern = "\\)|\\.", replacement = "")) |> 
  rename(barrier_other = finacial_barrier_other) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~str_replace(string = ., pattern = "\\)|\\.", replacement = ""))
         ) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~str_replace(string = ., pattern = "\\+", replacement = "_"))
         )

# tool
loc_tool <- "inputs/ETH2303_JRMA_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey") |> 
  mutate(name = str_replace(name, "_os$", "_other"),
         name = ifelse(name %in% c("finacial_barrier_other"), "barrier_other", name))

df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::English`,
         name = str_replace(string = name, pattern = "\\)|\\.", replacement = ""),
         name = str_replace(string = name, pattern = "\\+", replacement = "_"),
         )


# main dataset ------------------------------------------------------------
vars_to_remove_from_data = c("deviceid", "audit", "audit_URL", "instance_name",
                             "gps", "_gps_latitude", "_gps_longitude", "_gps_altitude",
                             "_gps_precision")

df_cleaning_log_main <-  df_cleaning_log 

df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                               input_df_survey = df_survey,
                                               input_df_choices = df_choices,
                                               input_df_cleaning_log = df_cleaning_log_main,
                                               input_vars_to_remove_from_data = vars_to_remove_from_data)


df_cleaned_data <- df_cleaning_step |> 
mutate(across(contains("/"), .fns = ~ as.character(.x)),
       across(contains("/"), .fns = ~ case_when( . %in% c("0") ~ "FALSE",
                                                 . %in% c("1") ~ "TRUE",
                                                 TRUE ~ .))
       )

# Add composite indicators at this stage ----------------------------------



# write final datasets out -----------------------------------------------

df_raw_data_final <- df_raw_data |> 
  mutate(across(.cols = any_of(vars_to_remove_from_data), .fns = ~na_if(., .)))

openxlsx::write.xlsx(x = df_raw_data_final,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_raw_data_eth_jrma_somali.xlsx"))

openxlsx::write.xlsx(x = df_cleaned_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_eth_jrma_somali.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")
