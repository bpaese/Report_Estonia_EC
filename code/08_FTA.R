##############################################################
# Gravity estimations - Regional Trade Agreement Effects
##############################################################

# SETUP -------------------------------------------------------------------

## Load packages
library(tidyverse)
library(here) # easy file referencing, root is set at project root
library(fixest) # perform estimations with multiple fixed-effects (fastest way to estimate Gravity equations)
library(huxtable) # Output tables

## Source user-written functions
source(here("code","03_Toolbox.R"))

## Load global variables
load(here("output","globalvariables.Rdata"))

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

## Dummy variable for the 1995 FTA
gravity_data <- gravity_data %>%
  mutate(FTA_1995 = ifelse(
          ((exporter_iso3 == "EST" & importer_iso3 %in% eu) | (exporter_iso3 %in% eu & importer_iso3 == "EST")) & (year >= 1995), 1, 0))

## Create fixed effects 
gravity_data <- gravity_data %>%
  unite("fe_exp_year", c(exporter_iso3, year), sep="_",remove=FALSE) %>%
  unite("fe_imp_year", c(importer_iso3, year), sep="_",remove=FALSE)

## Specify country-specific intra-national trade dummies
gravity_data <- gravity_data %>%
  mutate(D_trade_ii = ifelse(exporter_iso3 == importer_iso3, exporter_iso3, "international"))

# 1a. OLS FE Estimation ---------------------------------------------------------

fta_ols <- feols(ln_trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 |
                  fe_exp_year + fe_imp_year, 
                data = gravity_data %>%
                  filter(trade > 0 & exporter_iso3 != importer_iso3), 
                vcov = cluster ~ pair_id)
summary(fta_ols)

# 1b. Traditional estimation (PPML) -------------------------------------------

fta_poisson <- fepois(trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 |
                       fe_exp_year + fe_imp_year,
                      data = gravity_data %>%
                       filter(exporter_iso3 != importer_iso3), 
                     vcov = cluster ~ pair_id)
summary(fta_poisson)

# 2. Allowing for trade-diversion from domestic sales   -------------------

## Create international border dummy variable
gravity_data <- gravity_data %>%
  mutate(intl_brdr = ifelse(exporter_iso3 == importer_iso3, as.character(pair_id), "international"))

## Estimation
fta_poisson_intra <- fepois(trade ~ ln_distance + contiguity + common_language + common_colonizer + EST_EU_1995 + EU_EST_1995 |
                             fe_exp_year + fe_imp_year + D_trade_ii,
                           data = gravity_data, 
                           vcov = cluster ~ pair_id)
summary(fta_poisson_intra)

# 3. Addressing potential endogeneity of FTAs------------------------------

## Estimation 
fta_endo <- fepois(trade ~ EST_EU_1995 + EU_EST_1995 |
                    fe_exp_year + fe_imp_year + pair_id,
                  data = gravity_data, 
                  vcov = cluster ~ pair_id)

summary(fta_endo)

# 4. Testing for potential "reverse causality" --------

## Identify future RTAs
gravity_data <- gravity_data %>%
  group_by(exporter_iso3,importer_iso3) %>%
  mutate(EST_EU_1995_lead4 = tlead(EST_EU_1995,n=4,along_with = year)) %>%
  mutate(EU_EST_1995_lead4 = tlead(EU_EST_1995,n=4,along_with = year))

## Estimation
fta_lead <- fepois(trade ~ EST_EU_1995 + EU_EST_1995 + EST_EU_1995_lead4 + EU_EST_1995_lead4 |
                    fe_exp_year + fe_imp_year + pair_id,
                  data = gravity_data,
                  vcov = cluster ~ pair_id)

summary(fta_lead)

# 5. Allowing for potential non-linear and phasing-in effects ---------------
# These lags allow for heterogeneity over time

## Identify past FTAs
# gravity_data <- gravity_data %>%
#   group_by(exporter_iso3,importer_iso3) %>%
#   mutate(EST_EU_1995_lag4 = tlag(EST_EU_1995,n=4,along_with = year),
#          EST_EU_1995_lag8 = tlag(EST_EU_1995,n=8,along_with = year),
#          EST_EU_1995_lag12 = tlag(EST_EU_1995,n=12,along_with = year)) %>%
#   mutate(EU_EST_1995_lag4 = tlag(EU_EST_1995,n=4,along_with = year),
#          EU_EST_1995_lag8 = tlag(EU_EST_1995,n=8,along_with = year),
#          EU_EST_1995_lag12 = tlag(EU_EST_1995,n=12,along_with = year))
#   
# fta_lags1 <- fepois(trade ~ EST_EU_1995 + EST_EU_1995_lag4 + EST_EU_1995_lag8 + EST_EU_1995_lag12 |
#                     fe_exp_year + fe_imp_year + pair_id,
#                   data = gravity_data, 
#                   vcov = cluster ~ pair_id)
# 
# summary(fta_lags1)
# 
# fta_lags2 <- fepois(trade ~ EU_EST_1995 + EU_EST_1995_lag4 + EU_EST_1995_lag8 + EU_EST_1995_lag12 |
#                       fe_exp_year + fe_imp_year + pair_id,
#                     data = gravity_data, 
#                     vcov = cluster ~ pair_id)
# 
# summary(fta_lags2)

## Visualise the evolution over time
# coeff_fta_lags <- coefficients(fta_lags)
# coeff_fta_lags <- cbind.data.frame(coeff_fta_lags, confint(fta_lags))
# colnames(coeff_fta_lags) <- c("coeff", "lower_ci", "upper_ci")
# coeff_fta_lags = coeff_fta_lags %>%
#   rownames_to_column(var="variable") %>% 
#   mutate(variable = factor(variable,levels = unique(variable), ordered=TRUE)) %>% # Provide order for x-axis later
#   filter(variable %in% c("FTA_1995","FTA1995_lag4","FTA1995_lag8","FTA1995_lag12")) # Take the two dummies out of the table, because they are not needed
#   
# fig_fta_evo <- ggplot(coeff_fta_lags) +
#   geom_point(aes(x=variable,y=coeff)) +
#   geom_line(aes(x=variable,y=coeff,group=1),linetype=linetypes[2]) + # the data was grouped by variable, to draw a line accross variables we need to specify a homogenous group, for instance 1
#   geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci,x=variable), width=0.2) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),axis.title.y=element_text(angle = 0, vjust = 0.5)) +
#   ylab("Coefficients") +
#   xlab("Variable")

## fig_fta_evo
# ggsave(file=here("output","figures","fig_fta_evo.pdf"), plot=fig_fta_evo,
#        height=20/1.7,units="cm")

# 6. Addressing globalization effects -------------------------------------

## Create time-specific international border variables
# gravity_data <- gravity_data %>%
#   mutate(D_inter = ifelse(importer_iso3 != exporter_iso3, 1, 0),
#          intl_brdr_year = paste0("intl_brdr_",year)) %>%
#   pivot_wider(names_from="intl_brdr_year",
#               values_from="D_inter",
#               values_fill = 0)
# 
# fta_glob <- fepois(trade ~  EST_EU_1995 + EST_EU_1995_lag4 + EST_EU_1995_lag8 + EST_EU_1995_lag12 +
#                     intl_brdr_1992 + intl_brdr_1996 + intl_brdr_2000 + intl_brdr_2004 + intl_brdr_2008 + intl_brdr_2012 + intl_brdr_2016 |
#                     fe_exp_year + fe_imp_year + pair_id,
#                   data = gravity_data, 
#                   vcov = cluster ~ pair_id)
# 
# summary(fta_glob)

# Table overview ------------------------------------------------------------------

table_glob_gravity <-  huxreg("(1) OLS" = fta_ols,
                           "(2) PPML" = fta_poisson,
                           "(3) INTRA" = fta_poisson_intra,
                           "(4) ENDG" = fta_endo,
                           "(5) LEAD" = fta_lead,
                           coefs = c("ln Distance" = "ln_distance",
                                     "Contiguity" = "contiguity",
                                     "Colony" = "common_colonizer",
                                     "Common language" = "common_language",
                                     "Estonia-EU 1995" = "EST_EU_1995",
                                     "EU-Estonia 1995" = "EU_EST_1995",
                                     "Estonia-EU 1995 (t+4)" = "EST_EU_1995_lead4"),
                           stars = NULL,
                           note = "Notes: All estimates are obtained with data for the years 1992, 1996, 2000, 2004, 2008, 2012, and 2016, and use exporter-time and importer-time fixed effects. The estimates of the fixed effects are omitted for brevity. Columns (1) and (2) use data on international trade flows only. Column (1) applies the OLS estimator and column (2) uses the PPML estimator. Column (3) adds intra-national trade observations and uses country-specific dummies for internal trade. Column (4) adds pair fixed effects. The estimates of the pair fixed effects are omitted for brevity. Column (5) introduces FTA lead. Standard errors are clustered by country pair and are reported in parentheses.") %>%
  insert_row(c("Intra-national trade", "No", "No", "Yes", "Yes", "Yes"), after = 19) %>%
  set_top_border(1,everywhere,1) %>%
  set_number_format(18:19, everywhere, 0) %>%
  set_tb_borders(21,everywhere,1) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.5,rep(0.7/7,7))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Estimating the Effects of 1995 Free Trade Agreement between Estonia and the EU") %>%
  set_label("table_glob_gravity") %>%
  set_font_size(9)

table_glob_gravity[18,1] = c("logLik (in thousands)")
table_glob_gravity[18,-1] = as.numeric(table_glob_gravity[18,-1])/1000
table_glob_gravity[19,1] = c("AIC (in thousands)")
table_glob_gravity[19,-1] = as.numeric(table_glob_gravity[19,-1])/1000


width(table_glob_gravity) = 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(table_glob_gravity),file=here("output","tables","table_glob_gravity.tex"))