##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

## Load packages
library(tidyverse)
library(here) # easy file referencing, root is set at project root
library(writexl)
library(readxl)

# Gravity data ------------------------------------------------------------

## Load data 
itpde_data <- read_rds(here("input","usitc_itpd_e_r02.rds")) # USITC International Trade and Production Database for Estimation - Release 2
dgd_data <- read_rds(here("input","usitc_dgd.rds")) # USITC Dynamic Gravity Dataset
ria_data <- read_rds(here("input","jm_ria_combined.rds")) # Regional Integration Agreements Database
pwt <- read_xlsx(path = here("input","pwt1001.xlsx"), sheet = "Data") # Penn World Table version 10.01
ahs_tariff <- read_xlsx(path = here("input","AHS_tariffs.xlsx"), sheet = "Partner-Timeseries", range = "E1:AD2") # WITS database

## Data treatment
itpde_data <- itpde_data %>%
  filter(exporter_iso3 == "EST" | importer_iso3 == "EST")
dgd_data <- dgd_data %>%
  filter(iso3_o == "EST" | iso3_d == "EST")
ria_data <- ria_data %>%
  filter(O_Parent == "EC Estonia")
pwt <- pwt %>%
  filter(country == "Estonia" & year > 1991) %>%
  select(-c(countrycode, country, currency_unit))
ahs_tariff <- ahs_tariff %>%
  pivot_longer(cols = -`Indicator`, names_to = "year", values_to = "Value") %>%
  pivot_wider(names_from = `Indicator`, values_from = `Value`) %>%
  mutate(year = ymd(year, truncated = 2L)) %>%
  rename(AHS_tariff = `AHS Weighted Average (%)`)

## Save the datasets to .rds format
saveRDS(itpde_data, file=here("output","estonia_itpde.RDS"))
saveRDS(dgd_data, file=here("output","estonia_dgd.RDS"))
saveRDS(ria_data, file=here("output","estonia_ec_ria.RDS"))
saveRDS(pwt, file=here("output","estonia_pwt.RDS"))
saveRDS(ahs_tariff, file=here("output","estonia_ahs.RDS"))

## Save the datasets to .csv format
write_csv(itpde_data,file=here("output","estonia_itpde.csv"))
write_csv(dgd_data,file=here("output","estonia_dgd.csv"))
write_csv(ria_data,file=here("output","estonia_ec_ria.csv"))
write_csv(pwt,file=here("output","estonia_pwt.csv"))
write.csv(ahs_tariff, file=here("output","estonia_ahs.csv"))

# Remove useless datasets
rm(dgd_data, itpde_data, ria_data, pwt, ahs_tariff)
