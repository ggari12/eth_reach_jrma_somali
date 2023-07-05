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
         !name %in% vars_to_remove) |> 
  select(variable = name) |>
  bind_rows(tibble::tribble(~variable,
                            "i.price_change",
                            "i.price_increase_perc",
                            "i.price_decrease_perc",
                            "i.ret_price_change",
                            "i.ret_price_increase_perc",
                            "i.ret_price_decrease_perc",
                            "i.ret_reason_for_price_increase",
                            "i.ret_reason_for_price_decrease",
                            "i.ws_price_change",
                            "i.ws_price_increase_perc",
                            "i.ws_price_decrease_perc",
                            "i.ws_reason_for_price_increase",
                            "i.ws_reason_for_price_decrease",
                            "i.mitigating_factors",
                            "i.ret_mitigating_factors",
                            "i.ws_mitigating_factors",
                            "i.ret_change_of_supplier",
                            "i.ret_change_of_supplier_impact",
                            "i.ws_change_of_supplier",
                            "i.ws_change_of_supplier_impact",
                            "i.ws_supply_chain_barriers_yesno",
                            "i.ws_supply_chain_barriers",
                            "i.ret_price",
                            "i.ret_stock_days",
                            "i.ret_resupply_days",
                            "i.ret_meet_demand",
                            "i.ws_price",
                            "i.ws_stock_days",
                            "i.ws_resupply_days",
                            "i.ws_meet_demand"
  )) |> 

  mutate(split = "all",
         subset_1 = "zone1",
         subset_2 = "repondent_type") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# output r_dap
write_csv(x = df_dap_file_data_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_r_dap_eth_jrma.csv"), na = "NA") 
write_csv(x = df_dap_file_data_composites, file = "inputs/r_dap_eth_jrma.csv", na = "NA")
