library(tidyverse)
library(srvyr)
library(supporteR)  

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "inputs/clean_data_eth_jrma_somali.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_os$|_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, col_types = c_types, na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/ETH2303_JRMA_Somali_tool.xlsx", sheet = "survey") |> 
  mutate(name = str_replace(name, "_os$", "_other"),
         name = ifelse(name %in% c("finacial_barrier_other"), "barrier_other", name))

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_eth_jrma.csv", show_col_types = FALSE)

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data)

# analysis

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap |> 
                                                   filter(!variable %in% c("price_decrease_perc_wheat", 
                                                                           "ret_price_decrease_perc_wheat",
                                                                           "ret_price_decrease_perc_maize",
                                                                           "ret_price_decrease_perc_goat_meat",
                                                                           "ret_price_decrease_perc_cooking_oil",
                                                                           "ret_price_decrease_perc_laundary_soap",
                                                                           "ret_food_supplier_country",
                                                                           "ws_price_decrease_perc_tomato",
                                                                           "ws_accessibility_zone"))
                                                                          )
# merge analysis

combined_analysis <- df_main_analysis

# formatting the analysis, adding question labels
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(variable = ifelse(variable %in% c("i.fcs", "i.rcsi", "i.hhs"), str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% c("int.fcs", "int.rcsi", "int.hhs"), "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% c("i.fcs", "i.hhs") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  mutate(variable = ifelse(variable %in% c("int.fcs", "int.rcsi", "int.hhs"), str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% c("int.fcs", "int.rcsi", "int.hhs"), str_replace(string = label, pattern = "int.", replacement = "i."), label)) |> 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_eth_jrma_somali.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_eth_jrma_somali.csv"), na="")
