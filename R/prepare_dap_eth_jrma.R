# read packages
library(tidyverse)

df_tool_survey <- readxl::read_excel(path = "support_files/JRAM_analysis_plan.xlsx", sheet = "survey")

vars_to_remove <- c("consent",
                    "enumerator_id",
                    "woreda1",
                    "ws_enumerator_comments",
                    "gps"
)

df_dap_file_data_composites <- df_tool_survey |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple"),
         #str_detect(string = name, pattern = "^fs_hhs|^fs_fcs|^rCSI|^livh"),
         !name %in% vars_to_remove) |> 
  select(variable = name) |>

  mutate(split = "all",
         subset_1 = "zone1",
        #subset_2 = "repondent_type",
         subset_3 = "repondent_type") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# output r_dap
write_csv(x = df_dap_file_data_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_r_dap_eth_jrma.csv"), na = "NA") 
write_csv(x = df_dap_file_data_composites, file = "inputs/r_dap_eth_jrma.csv", na = "NA")
