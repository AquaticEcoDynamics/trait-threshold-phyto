library(tidyr)
library(dplyr)
library(readr)

# Read the CSV file
phyto_data <- read_csv("Data_HN_Phytoplankton_SWC_13_3_2025.csv")

# Clean up and select the needed columns
phyto_data <- phyto_data %>%
  select(`Genus Name`, `Site Code`, `Calendar Date`, `Cells/mL`)

# Pivot to wide format and fill NAs with 0
phyto_wide <- phyto_data %>%
  pivot_wider(
    names_from = `Genus Name`,
    values_from = `Cells/mL`,
    values_fill = list(`Cells/mL` = 0),
    values_fn = sum  # <- this handles duplicates
  )
# View the result
print(phyto_wide)
write.csv(phyto_wide, file = "Phyto_HN_wide.csv")


phyto_wide_log <- phyto_wide %>%
  mutate(across(
    .cols = -c(`Site Code`, `Calendar Date`),  # Exclude 'Site Code' and 'Calendar Date' columns
    .fns = ~ log10(. + 1)  # Apply log transformation to each species column
  ))

write.csv(phyto_wide_log, file = "Phyto_HN_wide_log.csv")

