library(tidyverse)
library(srvyr)
library(supporteR) 

source("R/composite_indicators.R")

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "inputs/clean_data_eth_jrma_somali.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_os$|_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, col_types = c_types, na = "NA") |> 
  create_composite_indicators() 

# tool
df_survey <- readxl::read_excel("inputs/ETH2303_JRMA_Somali_tool.xlsx", sheet = "survey") |> 
  mutate(name = str_replace(name, "_os$", "_other"),
         name = ifelse(name %in% c("finacial_barrier_other"), "barrier_other", name))

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "decimal|integer|date|select_one|select_multiple")) |> 
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
                                                                           "ws_accessibility_zone",
                                                                           "ret_credit_demand_per_customer",
                                                                           "ret_reason_for_price_decrease_cooking_oil",
                                                                           "ret_reason_for_price_decrease_goat_meat",
                                                                           "ret_reason_for_price_decrease_laundary_soap",
                                                                           "ret_reason_for_price_decrease_maize",
                                                                           "ret_reason_for_price_decrease_tomato",
                                                                           "ret_reason_for_price_decrease_wheat",
                                                                           "ret_reason_for_price_increase_cooking_oil",
                                                                           "ret_reason_for_price_increase_goat_meat",
                                                                           "ret_reason_for_price_increase_laundary_soap",
                                                                           "ret_reason_for_price_increase_maize",
                                                                           "ret_reason_for_price_increase_tomato",
                                                                           "ret_reason_for_price_increase_wheat",
                                                                           "ret_types_of_barriers_to_access_cash",
                                                                           "ws_credit_demand_per_customer",
                                                                           "ws_reason_for_price_decrease_cooking_oil",
                                                                           "ws_reason_for_price_decrease_goat_meat",
                                                                           "ws_reason_for_price_decrease_laundary_soap",
                                                                           "ws_reason_for_price_decrease_maize",
                                                                           "ws_reason_for_price_decrease_tomato",
                                                                           "ws_reason_for_price_decrease_wheat",
                                                                           "ws_reason_for_price_increase_cooking_oil",
                                                                           "ws_reason_for_price_increase_goat_meat",
                                                                           "ws_reason_for_price_increase_laundary_soap",
                                                                           "ws_reason_for_price_increase_maize",
                                                                           "ws_reason_for_price_increase_tomato",
                                                                           "ws_reason_for_price_increase_wheat",
                                                                           "i.ret_reason_for_price_increase",
                                                                           "i.ret_reason_for_price_decrease",
                                                                           "i.ws_reason_for_price_increase",
                                                                           "i.ws_reason_for_price_decrease"))
                                                                          )





# Please wait the following lines of code might takes 3/5 minutes
# merge analysis

combined_analysis <- df_main_analysis

# formatting the analysis, adding question labels
integer_cols_i <- c("i.ret_price", "i.ret_stock_days","i.ret_resupply_days","i.ws_price", "i.ws_stock_days","i.ws_resupply_days")
integer_cols_int <- c("int.ret_price", "int.ret_stock_days","int.ret_resupply_days","int.ws_price", "int.ws_stock_days","int.ws_resupply_days")

full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  mutate(variable = ifelse(variable %in% integer_cols_int, str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% integer_cols_int, str_replace(string = label, pattern = "int.", replacement = "i."), label)) |> 
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
