##############################################################
# Gravity estimations - Distance Puzzle
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

## Create fixed effects 
gravity_data <- gravity_data %>%
  unite("fe_exp_year", c(exporter_iso3, year), sep="_",remove=FALSE) %>%
  unite("fe_imp_year", c(importer_iso3, year), sep="_",remove=FALSE)

## Create a dummy for intra-national trade
## Create time-specific international distance variables
gravity_data <- gravity_data %>%
  mutate(ln_distance_year = paste0("ln_distance_",year),
         ln_distance_copy = ln_distance) %>%
  pivot_wider(names_from="ln_distance_year", 
              values_from="ln_distance_copy",
              values_fill = 0) 

# 1a. OLS Gravity -------------------------------------------------------------

## Estimation
distance_ols <- feols(ln_trade ~ ln_distance_1992 + ln_distance_1996 + ln_distance_2000 + ln_distance_2004 + ln_distance_2008 + ln_distance_2012 + ln_distance_2016 +
                        EST_EU_1995 + EU_EST_1995 + contiguity + common_language + common_colonizer |
                        fe_exp_year + fe_imp_year,
                      data = gravity_data %>% filter(trade > 0 & exporter_iso3 != importer_iso3),
                      vcov = cluster ~ pair_id)
summary(distance_ols)

# 1b. PPML Gravity ------------------------------------------------------------
distance_poisson <- fepois(trade ~ ln_distance_1992 + ln_distance_1996 + ln_distance_2000 + ln_distance_2004 + ln_distance_2008 + ln_distance_2012 + ln_distance_2016 +
                             EST_EU_1995 + EU_EST_1995 + contiguity + common_language + common_colonizer |
                            fe_exp_year + fe_imp_year,
                          data = gravity_data %>% filter(exporter_iso3 != importer_iso3),
                          vcov = cluster ~ pair_id)
summary(distance_poisson)

# 2. Internal distance solution for the distance puzzle -----------------------

## Create dummy variable for intra-national trade
## Create intra-national distance variable 
## Set international distance variable to zero for intra-national trade to avoid overlap
gravity_data <- gravity_data %>%
  mutate(D_intra = ifelse(exporter_iso3 == importer_iso3, 1, 0),
         ln_distance_intra = ln_distance * D_intra,
         across(ln_distance_1992:ln_distance_2016, ~ ifelse(D_intra==1,0,.x)))

distance_poisson_intra <- fepois(trade ~ ln_distance_1992 + ln_distance_1996 + ln_distance_2000 + ln_distance_2004 + ln_distance_2008 + ln_distance_2012 + ln_distance_2016 +
                                   EST_EU_1995 + EU_EST_1995 + contiguity + common_language + common_colonizer + ln_distance_intra |
                                  fe_exp_year + fe_imp_year,
                                data = gravity_data,
                                vcov = cluster ~ pair_id)

summary(distance_poisson_intra)

# 3. Internal distance and home bias solution for the distance puzzle --------

distance_poisson_intra_home <- fepois(trade ~ ln_distance_1992 + ln_distance_1996 + ln_distance_2000 + ln_distance_2004 + ln_distance_2008 + ln_distance_2012 + ln_distance_2016 +
                                        EST_EU_1995 + EU_EST_1995 + contiguity + common_language + common_colonizer + ln_distance_intra + D_intra |
                                        fe_exp_year + fe_imp_year,
                                     data = gravity_data,
                                     vcov = cluster ~ pair_id)
summary(distance_poisson_intra_home)

# 4. Fixed effects solution for the "distance puzzle" ---------------------

## Specify country-specific intra-national trade dummies
gravity_data <- gravity_data %>%
  mutate(D_trade_ii = ifelse(exporter_iso3 == importer_iso3, exporter_iso3, "international"))

distance_poisson_intra_fe <- fepois(trade ~ ln_distance_1992 + ln_distance_1996 + ln_distance_2000 + ln_distance_2004 + ln_distance_2008 + ln_distance_2012 + ln_distance_2016 +
                                      EST_EU_1995 + EU_EST_1995 + contiguity + common_language + common_colonizer |
                                     fe_exp_year + fe_imp_year + D_trade_ii,
                                   data = gravity_data,
                                   vcov = cluster ~ pair_id)
summary(distance_poisson_intra_fe)

# Table overview ------------------------------------------------------------------

table_distance_gravity <-  huxreg("(1) OLS" = distance_ols,
                               "(2) PPML" = distance_poisson,
                               "(3) INTRA" = distance_poisson_intra,
                               "(4) BRDR" = distance_poisson_intra_home,
                               "(5) FEs" = distance_poisson_intra_fe,
                               coefs = c("ln Distance 1992" = "ln_distance_1992",
                                         "ln Distance 1996" = "ln_distance_1996",
                                         "ln Distance 2000" = "ln_distance_2000",
                                         "ln Distance 2004" = "ln_distance_2004",
                                         "ln Distance 2008" = "ln_distance_2008",
                                         "ln Distance 2012" = "ln_distance_2012",
                                         "ln Distance 2016" = "ln_distance_2016",
                                         "Contiguity" = "contiguity",
                                         "Colony" = "common_colonizer",
                                         "Common language" = "common_language",
                                         "Estonia-EU 1995" = "EST_EU_1995",
                                         "EU-Estonia 1995" = "EU_EST_1995",
                                         "ln intra-national distance" = "ln_distance_intra",
                                         "Intra-national trade dummy" = "D_intra"),
                               stars = NULL,
                               note = "Notes: All estimates are obtained with data for the years 1992, 1996, 2000, 2004, 2008, 2012, and 2016, and use exporter-time and importer-time fixed effects. The estimates of the fixed effects are omitted for brevity. Columns (1) and (2) use data on international trade flows only. Column (1) employs the OLS estimator and column (2) uses the PPML estimator. Column (3) adds internal trade observations and uses intra-national distance as an additional covariate. Column (4) adds an indicator covariate for international trade. Finally, column (5) uses country-specific dummies for intra-national trade. Standard errors are clustered by country pair and are reported in parentheses.") %>%
  insert_row(c("Intra-national trade", "No", "No", "Yes", "Yes","Yes"),
             after = 33) %>%
  insert_row(c("Country-specific intra-national fixed effects", "No", "No", "No", "No", "Yes"),
             after = 34) %>%
  set_number_format(32:33, everywhere, 0) %>%
  set_tb_borders(33:34,everywhere,0) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.35,rep(0.65/5,5))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Solution to the ``Distance Puzzle'' in Trade") %>%
  set_label("table_distance_gravity")  %>%
  set_font_size(8)

table_distance_gravity[32,1] <- c("logLik (in thousands)")
table_distance_gravity[32,-1] <- as.numeric(table_distance_gravity[32,-1])/1000
table_distance_gravity[33,1] <- c("AIC (in thousands)")
table_distance_gravity[33,-1] <- as.numeric(table_distance_gravity[33,-1])/1000

number_format(table_distance_gravity)[32:33,] <- 2
width(table_distance_gravity) <- 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(table_distance_gravity),file=here("output","tables","table_distance_gravity.tex"))