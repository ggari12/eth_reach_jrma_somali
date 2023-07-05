# creating composite indicators -------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df |>
    rowwise() |> 
    mutate(i.price_change = mode_with_out_nc(c_across(price_change_wheat:price_change_laundary_soap), na.rm = T),
           i.price_increase_perc = mode_with_out_nc(c_across(price_increase_perc_wheat:price_increase_perc_laundary_soap), na.rm = T),
           i.price_decrease_perc = mode_with_out_nc(c_across(price_decrease_perc_wheat:price_decrease_perc_laundary_soap), na.rm = T),
           i.ret_price_change = mode_with_out_nc(c_across(ret_price_change_wheat:ret_price_change_laundary_soap), na.rm = T),
           i.ret_price_increase_perc = mode_with_out_nc(c_across(ret_price_increase_perc_wheat:ret_price_increase_perc_laundary_soap), na.rm = T),
           i.ret_price_decrease_perc = mode_with_out_nc(c_across(ret_price_decrease_perc_wheat:ret_price_decrease_perc_laundary_soap), na.rm = T),
           i.ret_reason_for_price_increase = mode_with_out_nc(c_across(ret_reason_for_price_increase_wheat:ret_reason_for_price_increase_laundary_soap), na.rm = T),
           i.ret_reason_for_price_decrease = mode_with_out_nc(c_across(ret_reason_for_price_decrease_wheat:ret_reason_for_price_decrease_laundary_soap), na.rm = T),
           i.ws_price_change = mode_with_out_nc(c_across(ws_price_change_wheat:ws_price_change_laundary_soap), na.rm = T),
           i.ws_price_increase_perc = mode_with_out_nc(c_across(ws_price_increase_perc_wheat:ws_price_increase_perc_laundary_soap), na.rm = T),
           i.ws_price_decrease_perc = mode_with_out_nc(c_across(ws_price_decrease_perc_wheat:ws_price_decrease_perc_laundary_soap), na.rm = T),
           i.ws_reason_for_price_increase = mode_with_out_nc(c_across(ws_reason_for_price_increase_wheat:ws_reason_for_price_increase_laundary_soap), na.rm = T),
           i.ws_reason_for_price_decrease = mode_with_out_nc(c_across(ws_reason_for_price_decrease_wheat:ws_reason_for_price_decrease_laundary_soap), na.rm = T),
           i.mitigating_factors = mode_with_out_nc(c_across(mitigating_factors_wheat:mitigating_factors_laundary_soap), na.rm = T),
           i.ret_mitigating_factors = mode_with_out_nc(c_across(ret_mitigating_factors_wheat:ret_mitigating_factors_laundary_soap), na.rm = T),
           i.ws_mitigating_factors = mode_with_out_nc(c_across(ws_mitigating_factors_wheat:ws_mitigating_factors_laundary_soap), na.rm = T)
    ) |>
    ungroup()
}
