##############################################################
# Gravity estimation - Traditional Estimates
##############################################################

# SETUP -------------------------------------------------------------------

## Load packages
library(tidyverse)
library(here) # easy file referencing, root is set at project root
library(fixest) # perform estimations with multiple fixed-effects (fastest way to estimate Gravity equations)
library(huxtable) # Output tables

## Load data 
itpde_data <- read_rds(here("output","usitc_itpd_e_r02.rds")) # USITC International Trade and Production Database for Estimation - Release 2
dgd_data <- read_rds(here("output","usitc_dgd.rds")) # USITC Dynamic Gravity Dataset

# Data preparation --------------------------------------------------------
## Merge datasets
gravity_data <- inner_join(itpde_data, dgd_data, by = c("year", "exporter_dynamic_code"="dynamic_code_o", "importer_dynamic_code"="dynamic_code_d"))

## Filter and clean data
gravity_data <- gravity_data %>%
  filter(year %in% seq(1992, 2019, 4))
  
## Construct symmetric pair id's
gravity_data <- gravity_data %>%
  mutate(pair = paste(pmin(exporter_iso3, importer_iso3),pmax(exporter_iso3, importer_iso3),sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## Construct exporter output and importer expenditure
gravity_data <- gravity_data %>%
  group_by(exporter_iso3, year) %>%
  mutate(Y_it = sum(trade)) %>%
  group_by(importer_iso3, year) %>%
  mutate(E_jt = sum(trade))

## Calculate the necessary logs
gravity_data <- gravity_data %>%
  mutate(across(c(trade, distance, gdp_pwt_const_o, gdp_pwt_const_d, Y_it, E_jt), ~log(.x), .names="ln_{.col}"))

eu <- c("BEL", "LUX", "DEU", "FRA", "ITA", "NLD", "DNK", "IRL", "GBR", "GRC", "ESP", "PRT", "AUT", "FIN", "SWE", "CYP", "CZE", "HUN", "LVA", "LTU", "MLT", "POL", "SVK", "SVN")

## Country-pair dummy variable for bilateral trade between Estonia and EU after 1995 
gravity_data <- gravity_data %>%
  mutate(
    EST_EU_1995 = ifelse(
      (exporter_iso3 == "EST" & importer_iso3 %in% eu) & (year >= 1995), 1, 0),
    EU_EST_1995 = ifelse(
      (exporter_iso3 %in% eu & importer_iso3 == "EST") & (year >= 1995), 1, 0))

## Country-pair dummy variable for bilateral trade between Estonia and EU after 1998
gravity_data <- gravity_data %>%
  mutate(
    EST_EU_1998 = ifelse(
      (exporter_iso3 == "EST" & importer_iso3 %in% eu) & (year >= 1998), 1, 0),
    EU_EST_1998 = ifelse(
      (exporter_iso3 %in% eu & importer_iso3 == "EST") & (year >= 1998), 1, 0))

# Extract the dummies created above to use as a control
#dummies_control <- select(gravity_data, exporter_iso3, importer_iso3, year, EST_EU_1995, EST_EU_1998, EU_EST_1995, EU_EST_1998)

# 1. Naive Gravity --------------------------------------------------------

fit_ols <- feols(ln_trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 + EST_EU_1998 + EU_EST_1998 + ln_Y_it + ln_E_jt + member_wto_o + member_wto_d + member_eu_o + member_eu_d + agree_fta,
                data = gravity_data %>%
                  filter(trade > 0 & exporter_iso3 != importer_iso3), 
                vcov = cluster ~ pair_id) # allows for cluster correlations
summary(fit_ols)

# 2. Proxy for multilateral resistance ------------------------------------

## Calculate remoteness indices
gravity_data <- gravity_data %>%
  group_by(year) %>%
  mutate(Y_t = sum(Y_it), E_t = sum(E_jt)) %>%
  group_by(exporter_iso3, year) %>%
  mutate(remoteness_exp = sum(distance /(E_jt / E_t))) %>%
  group_by(importer_iso3, year) %>%
  mutate(remoteness_imp = sum(distance / (Y_it / Y_t))) %>%
  mutate(ln_remoteness_exp = log(remoteness_exp), 
         ln_remoteness_imp = log(remoteness_imp))

## Estimation
fit_remoteness <- feols(ln_trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 + EST_EU_1998 + EU_EST_1998 + ln_Y_it + ln_E_jt + member_wto_o + member_wto_d + member_eu_o + member_eu_d + agree_fta + ln_remoteness_exp + ln_remoteness_imp,
                       data = gravity_data %>% 
                         filter(trade > 0 & exporter_iso3 != importer_iso3),
                       vcov = cluster ~ pair_id)
summary(fit_remoteness)

# 3. Fixed Effects -----------------------------------------------------------

## Create fixed effects 
gravity_data <- gravity_data %>%
  unite("fe_exp_year", c(exporter_iso3, year), sep="_",remove=FALSE) %>%
  unite("fe_imp_year", c(importer_iso3, year), sep="_",remove=FALSE)

## Estimation
fit_fixedeffects <- feols(ln_trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 + EST_EU_1998 + EU_EST_1998 + member_wto_o + member_wto_d + member_eu_o + member_eu_d + agree_fta |
                           fe_exp_year + fe_imp_year,
                         data = gravity_data %>% 
                           filter(trade > 0 & exporter_iso3 != importer_iso3),
                         vcov = cluster ~ pair_id)
summary(fit_fixedeffects)

# 4. Fit gravity with PPML and fixed effects -----------------------------

fit_poisson <- fepois(trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 + EST_EU_1998 + EU_EST_1998 + member_wto_o + member_wto_d + member_eu_o + member_eu_d + agree_fta |
                        fe_exp_year + fe_imp_year,
                      data = gravity_data %>% 
                        filter(exporter_iso3 != importer_iso3),
                      vcov = cluster ~ pair_id)
summary(fit_poisson)

# Table overview ----------------------------------------------------------------

table_traditional_gravity <- huxreg("Naive" = fit_ols,
                                  "Remoteness" = fit_remoteness,
                                  "Fixed Effects" = fit_fixedeffects,
                                  "Fixed Effects" = fit_poisson,
                                  coefs = c("Intercept" = "(Intercept)",
                                            "ln Distance" = "ln_distance",
                                            "Contiguity" = "contiguity",
                                            "Common language" = "common_language",
                                            "Colony" = "common_colonizer",
                                            "Estonia-EU 1995" = "EST_EU_1995",
                                            "EU-Estonia 1995" = "EU_EST_1995",
                                            "Estonia-EU 1998" = "EST_EU_1998",
                                            "EU-Estonia 1998" = "EU_EST_1998",
                                            "WTO origin" = "member_wto_o",
                                            "WTO destination" = "member_wto_d",
                                            "EU origin" = "member_eu_o",
                                            "EU destination" = "member_eu_d",
                                            "FTA" = "agree_fta",
                                            "ln Output" = "ln_Y_it",
                                            "ln Expenditure" = "ln_E_jt",
                                            "Exporter remoteness" = "ln_remoteness_exp",
                                            "Importer remoteness" = "ln_remoteness_imp"),
                                   stars = NULL,
                                   note = "Notes: Statistics based on author's calculations. All estimates are obtained with data for the years 1992, 1996, 2000, 2004, 2008, 2012, and 2016. Standard errors are clustered by country pair and are reported in parentheses."
) %>%
  insert_row("","(1) OLS", "(2) OLS", "(3) OLS", "(4) PPML", after = 0) %>%
  set_top_border(1,everywhere,1) %>%
  set_align(1, everywhere, "center") %>%
  insert_row(c("Exporter-time fixed effects", "No", "No", "Yes", "Yes"),
             after = 42) %>%
  insert_row(c("Importer-time fixed effects", "No", "No", "Yes", "Yes"),
             after = 43) %>%
  set_number_format(39:40, everywhere, 0) %>%
  set_tb_borders(42:43,everywhere,0) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3,rep(0.7/4,4))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Traditional Gravity Estimates") %>%
  set_label("table_traditional_gravity") %>%
  set_height("75%") %>%
  set_font_size(9)

width(table_traditional_gravity) <- 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(table_traditional_gravity),file=here("output","tables","table_traditional_gravity.tex"))