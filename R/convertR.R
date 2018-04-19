source('convertR_functions.R')

# EPPA input file
eppa <- '/chm_e5_tax50.put'

eppa_data <- eppa_to_tibble(eppa)

data <- eppa_data %>%
  # Convert CO2 to Gt
  mutate(value = if_else(gas == "CO2", value / 1000, value)) %>%
  # Changes to gas names to match GCAM
  mutate(gas = if_else(gas == "CO2" & variable == "n_ag", "ffi_emissions", gas),
         gas = if_else(gas == "CO2" & variable == "agr", "luc_emissions", gas),
         gas = sub("HFC", "HFC134a", gas),
         gas = sub("PFC", "CF4", gas),
         gas = sub("amo", "NH3", gas),
         gas = sub("VOC", "NMVOC", gas),
         gas = if_else(!(gas %in% c("ffi_emissions", "luc_emissions", "NH3")),
                         paste0(gas, "_emissions"), gas)) %>%
  # Aggregate globally and across variables (agr vs non-agr)
  group_by(year, gas) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(gas, value) %>%
  write_csv("output/eppa_emissions_for_hector.csv")
