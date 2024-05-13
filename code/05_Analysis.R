##############################################################
# Descriptive analysis
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(here) #easy file referencing, root is set at project root
#library(flextable) #Output tables to word
library(huxtable) # Outupt tables

# Load global variables
load(here("output","globalvariables.Rdata"))

# Defining variables ---------------------------------------------------------

# eu1 are the EU countries before the 2004 enlargement
eu1 <- c("BEL", "LUX", "DEU", "FRA", "ITA", "NLD", "DNK", "IRL", "GBR", "GRC", "ESP", "PRT", "AUT", "FIN", "SWE")
# eu2 are the EU countries after the 2004 enlargement
eu2 <- c("BEL", "LUX", "DEU", "FRA", "ITA", "NLD", "DNK", "IRL", "GBR", "GRC", "ESP", "PRT", "AUT", "FIN", "SWE", "CYP", "CZE", "HUN", "LVA", "LTU", "MLT", "POL", "SVK", "SVN")

## Load data
estonia_itpde <- readRDS(file=here("output","estonia_itpde.RDS"))
#estonia_ec_ria <- readRDS(file=here("output","estonia_ec_ria.RDS"))
estonia_pwt <- readRDS(file=here("output","estonia_pwt.RDS"))
estonia_ahs <- readRDS(file=here("output","estonia_ahs.RDS"))

## Extract subsets
# Estonia's imports from the rest of the world (RoW)
estonia_imports <- estonia_itpde %>%
  filter(importer_iso3 == "EST" & exporter_iso3 != "EST")
# Estonia's exports to RoW
estonia_exports <- estonia_itpde %>%
  filter(exporter_iso3 == "EST" & importer_iso3 != "EST")
# Estonia's domestic trade flows
estonia_domestic_trade <- estonia_itpde %>%
  filter(exporter_iso3 == "EST" & importer_iso3 == "EST")
estonia_gdp <- estonia_pwt %>%
  select(year, rgdpna)

rm(estonia_itpde)

## Overall Openness
estonia_total_exp <- estonia_exports %>%
  group_by(year) %>%
  summarise(exports = sum(trade))
estonia_total_imp <- estonia_imports %>%
  group_by(year) %>%
  summarise(imports = sum(trade))
estonia_overall_openness <- tibble(year = estonia_total_exp$year,
                     Exports = estonia_total_exp$exports,
                     Imports = estonia_total_imp$imports,
                     GDP = estonia_gdp$rgdpna)
estonia_overall_openness <- estonia_overall_openness %>%
  mutate(total_trade = Exports + Imports, overall_openness = total_trade/GDP)

#
#
## Analysis of main trading partners (mtp)
mtp_exp_side1 <- estonia_exports %>%
  group_by(importer_iso3) %>%
  summarise(trade = sum(trade)) %>%
  arrange(desc(trade))

mtp_exp_side2 <- estonia_exports %>%
  group_by(importer_iso3, year) %>%
  summarise(trade = sum(trade)) %>%
  filter(importer_iso3 == "FIN" | importer_iso3 == "SWE" | importer_iso3 == "LVA" | importer_iso3 == "RUS" | importer_iso3 == "DEU" | importer_iso3 == "LTU") %>%
  rename(Country = importer_iso3)

mtp_exp_shares <- inner_join(estonia_total_exp, mtp_exp_side2, by = "year")
mtp_exp_shares <- mtp_exp_shares %>%
  mutate(share = trade/exports) %>%
  select(c(year, Country, share))

mtp_exp_shares_ranking <- mtp_exp_shares %>%
  group_by(year) %>%
  summarise(share = sum(share))

# mtp_exp_side3 <- estonia_exports %>%
#   group_by(importer_iso3, year) %>%
#   summarise(trade = sum(trade)) %>%
#   filter(importer_iso3 == "FIN" | importer_iso3 == "SWE" | importer_iso3 == "LVA" | importer_iso3 == "RUS" | importer_iso3 == "DEU" | importer_iso3 == "LTU" | importer_iso3 == "USA" | importer_iso3 == "GBR" | importer_iso3 == "NOR" | importer_iso3 == "NLD") %>%
#   rename(Country = importer_iso3) %>%
#   pivot_wider(names_from = Country, values_from = trade)

mtp_imp_side1 <- estonia_imports %>%
  group_by(exporter_iso3) %>%
  summarise(trade = sum(trade)) %>%
  arrange(desc(trade))

mtp_imp_side2 <- estonia_imports %>%
  group_by(exporter_iso3, year) %>%
  summarise(trade = sum(trade)) %>%
  filter(exporter_iso3 == "FIN" | exporter_iso3 == "SWE" | exporter_iso3 == "CHN" | exporter_iso3 == "RUS" | exporter_iso3 == "DEU" | exporter_iso3 == "LTU") %>%
  rename(Country = exporter_iso3)

mtp_imp_shares <- inner_join(estonia_total_imp, mtp_imp_side2, by = "year")
mtp_imp_shares <- mtp_imp_shares %>%
  mutate(share = trade/imports) %>%
  select(c(year, Country, share))

mtp_imp_shares_ranking <- mtp_imp_shares %>%
  group_by(year) %>%
  summarise(share = sum(share))

# mtp_imp_side3 <- estonia_imports %>%
#   group_by(exporter_iso3, year) %>%
#   summarise(trade = sum(trade)) %>%
#   filter(exporter_iso3 == "FIN" | exporter_iso3 == "SWE" | exporter_iso3 == "CHN" | exporter_iso3 == "RUS" | exporter_iso3 == "DEU" | exporter_iso3 == "LTU" | exporter_iso3 == "LVA" | exporter_iso3 == "POL" | exporter_iso3 == "GBR" | exporter_iso3 == "NLD") %>%
#   rename(Country = exporter_iso3) %>%
#   pivot_wider(names_from = Country, values_from = trade)

# mtp_exp_side3 and mtp_imp_side3 are used to build a table that we decide to leave out of the analysis

## Analysis of main sectors (in terms of broad sector classifications)
# sectors_exp_side_ranking1<- estonia_exports %>%
#   group_by(broad_sector) %>%
#   summarise(trade = sum(trade)) %>%
#   arrange(desc(trade))
# 
# sectors_exp_side1 <- estonia_exports %>%
#   group_by(broad_sector, year) %>%
#   summarise(trade = sum(trade)) %>%
#   rename(`Broad Sector` = broad_sector)
# 
# sectors_imp_side_ranking1 <- estonia_imports %>%
#   group_by(broad_sector) %>%
#   summarise(trade = sum(trade)) %>%
#   arrange(desc(trade))
# 
# sectors_imp_side1 <- estonia_imports %>%
#   group_by(broad_sector, year) %>%
#   summarise(trade = sum(trade)) %>%
#   rename(`Broad Sector` = broad_sector)

#
#
## Analysis of main sectors (in terms of broad sector classifications) in the relationship Estonia-EU
# sectors_exp_side_ranking2 <- estonia_exports %>%
#   filter(importer_iso3 %in% eu2) %>%
#   group_by(broad_sector) %>%
#   summarise(trade = sum(trade)) %>%
#   arrange(desc(trade))
# 
# sectors_exp_side2 <- estonia_exports %>%
#   filter(importer_iso3 %in% eu2) %>%
#   group_by(broad_sector, year) %>%
#   summarise(trade = sum(trade)) %>%
#   rename(`Broad Sector` = broad_sector)
# 
# agriculture_exp_side <- filter(sectors_exp_side2, `Broad Sector` == "Agriculture")
# 
# sectors_imp_side_ranking2 <- estonia_imports %>%
#   filter(exporter_iso3 %in% eu2) %>%
#   group_by(broad_sector) %>%
#   summarise(trade = sum(trade)) %>%
#   arrange(desc(trade))
# 
# sectors_imp_side2 <- estonia_imports %>%
#   filter(exporter_iso3 %in% eu2) %>%
#   group_by(broad_sector, year) %>%
#   summarise(trade = sum(trade)) %>%
#   rename(`Broad Sector` = broad_sector)
# 
# agriculture_imp_side <- filter(sectors_imp_side2, `Broad Sector` == "Agriculture")

#
#
## Bilateral trade flows between Estonia and European Community
# Estonia exports to EU
estonia_eu1 <- estonia_exports %>%
  filter(importer_iso3 %in% eu1, year < 2004) # filter EU countries before the 2004 enlargement, in the period 1992-2003

# estonia_eu1 are Estonia's yearly total exports to the EU, before the 2004 enlargement, in the period 1992-2003
estonia_eu1 <- estonia_eu1 %>%
  group_by(year) %>%
  summarise(trade = sum(trade))

estonia_eu2 <- estonia_exports %>%
  filter(importer_iso3 %in% eu2, year > 2003) # filter EU countries after the 2004 enlargement, in the period 2004-2019

# estonia_eu2 are Estonia's yearly total exports to the EU, after the 2004 enlargement, in the period 2004-2019
estonia_eu2 <- estonia_eu2 %>%
  group_by(year) %>%
  summarise(trade = sum(trade))

# Estonia imports from EU
eu1_estonia <- estonia_imports %>%
  filter(exporter_iso3 %in% eu1, year < 2004)

# eu1_estonia are Estonia's yearly total imports from the EU, before the 2004 enlargement, in the period 1992-2003
eu1_estonia <- eu1_estonia %>%
  group_by(year) %>%
  summarise(trade = sum(trade))

eu2_estonia <- estonia_imports %>%
  filter(exporter_iso3 %in% eu2, year > 2003)

# eu2_estonia are Estonia's yearly total imports from the EU, after the 2004 enlargement, in the period 2004-2019
eu2_estonia <- eu2_estonia %>%
  group_by(year) %>%
  summarise(trade = sum(trade))

# bilateral_flows1 are Estonia's yearly trade flows - exports and imports - with the EU, before the 2004 enlargement, in the period 1992-2003
bilateral_flows1 <- tibble(year = estonia_eu1$year,
                           `Exports to EU` = estonia_eu1$trade,
                           `Imports from EU` = eu1_estonia$trade)

# bilateral_flows2 are Estonia's yearly trade flows - exports and imports - with the EU, after the 2004 enlargement, in the period 2004-2019
bilateral_flows2 <- tibble(year = estonia_eu2$year,
                           `Exports to EU` = estonia_eu2$trade,
                           `Imports from EU` = eu2_estonia$trade)

rm(estonia_total_exp, estonia_total_imp, estonia_pwt, estonia_gdp, estonia_eu1, estonia_eu2, eu1_estonia, eu2_estonia)

# Graphs --------------------------------------------------------------------

graph_out <- ggplot(data = mtp_exp_shares) +
  geom_line(mapping = aes(x = year, y = share, colour = Country), linewidth = 1) +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  scale_color_manual(values = colorpalette) +
  scale_linetype_manual(values = linetypes) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Main Trading Partners, Export Side",
       subtitle = "Shares in total exports, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","EST_exports_partners.pdf"), plot = graph_out, width = 18, height = 8, units = "cm")
rm(graph_out)

graph_out <- ggplot(data = mtp_imp_shares) +
  geom_line(mapping = aes(x = year, y = share, colour = Country), linewidth = 1) +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  scale_color_manual(values = colorpalette) +
  scale_linetype_manual(values = linetypes) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Main Trading Partners, Import Side",
       subtitle = "Shares in total imports, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","EST_imports_partners.pdf"), plot = graph_out, width = 18, height = 8, units = "cm")
rm(graph_out)

ggplot(data = mtp_exp_shares_ranking) +
  geom_col(mapping = aes(x = year, y = share), fill = "dodgerblue3") +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Total Share of Main Trading Partners, Export Side",
       subtitle = "Sum of the shares in total exports, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))

ggplot(data = mtp_imp_shares_ranking) +
  geom_col(mapping = aes(x = year, y = share), fill = "dodgerblue3") +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Total Share of Main Trading Partners, Import Side",
       subtitle = "Sum of the shares in total imports, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))

# graph_out <- ggplot(data = sectors_exp_side2) +
#   geom_line(mapping = aes(x = year, y = trade, colour = `Broad Sector`), linewidth = 1) +
#   scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
#   scale_color_manual(values = colorpalette) +
#   scale_linetype_manual(values = linetypes) +
#   xlab("") +
#   ylab("") +
#   labs(title = "Estonia's Trade in Terms of Broad Sector Classifications, Export Side",
#        subtitle = "Bilateral trade flows Estonia-EU, millions of current US dollars, 1992-2019") +
#   theme_light() +
#   theme(plot.title = element_text(family = NULL, face = "bold"))
# graph_out
# ggsave(filename = here("output","figures","EST_exports_sectors.pdf"), plot = graph_out, width = 18, height = 8, units = "cm")
# rm(graph_out)
# 
# graph_out <- ggplot(data = agriculture_exp_side) +
#   geom_line(mapping = aes(x = year, y = trade), linewidth = 1) +
#   scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
#   scale_color_manual(values = colorpalette) +
#   scale_linetype_manual(values = linetypes) +
#   xlab("") +
#   ylab("") +
#   labs(title = "Estonia's Trade in Terms of the Agricultural Sector, Export Side",
#        subtitle = "Bilateral trade flows Estonia-EU, millions of current US dollars, 1992-2019") +
#   theme_light() +
#   theme(plot.title = element_text(family = NULL, face = "bold"))
# graph_out
# ggsave(filename = here("output","figures","EST_exports_agriculture.pdf"), plot = graph_out)
# rm(graph_out)
# 
# graph_out <- ggplot(data = agriculture_imp_side) +
#   geom_line(mapping = aes(x = year, y = trade), linewidth = 1) +
#   scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
#   scale_color_manual(values = colorpalette) +
#   scale_linetype_manual(values = linetypes) +
#   xlab("") +
#   ylab("") +
#   labs(title = "Estonia's Trade in Terms of the Agricultural Sector, Import Side",
#        subtitle = "Bilateral trade flows Estonia-EU, millions of current US dollars, 1992-2019") +
#   theme_light() +
#   theme(plot.title = element_text(family = NULL, face = "bold"))
# graph_out
# ggsave(filename = here("output","figures","EST_imports_agriculture.pdf"), plot = graph_out)
# rm(graph_out)
# 
# graph_out <- ggplot(data = sectors_imp_side2) +
#   geom_line(mapping = aes(x = year, y = trade, colour = `Broad Sector`), linewidth = 0.5) +
#   scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
#   xlab("") +
#   ylab("") +
#   labs(title = "Estonia's Trade in Terms of Broad Sector Classifications, Import Side",
#        subtitle = "Trade flows in millions of current US dollars, 1992-2019") +
#   theme_light() +
#   theme(plot.title = element_text(family = NULL, face = "bold"))
# graph_out
# ggsave(filename = here("output","figures","EST_imports_sectors.pdf"), plot = graph_out, width = 18, height = 8, units = "cm")
# rm(graph_out)

graph_out <- ggplot(data = estonia_overall_openness) +
  geom_line(mapping = aes(x = year, y = Exports, color = "Exports"), linewidth = 1) +
  geom_line(mapping = aes(x = year, y = Imports, color = "Imports"), linewidth = 1) +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  labs(color = NULL) +
  scale_color_manual(values = colorpalette) +
  scale_linetype_manual(values = linetypes) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Total Exports and Imports",
       subtitle = "Trade flows in millions of current US dollars, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","EST_total_trade.pdf"), plot = graph_out)
rm(graph_out)

graph_out <- ggplot(data = estonia_overall_openness) +
  geom_line(mapping = aes(x = year, y = overall_openness), color = "grey50", linewidth = 1) +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  labs(color = NULL) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Overall Openness",
       subtitle = "Trade over GDP, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","EST_overall_openness.pdf"), plot = graph_out)
rm(graph_out)

graph_out <- ggplot(data = estonia_ahs) +
  geom_line(mapping = aes(x = year, y = AHS_tariff), color = "grey50", linewidth = 0.5) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + #this scale is different than the others because the year in this data is a date object
  labs(color = NULL) +
  xlab("") +
  ylab("") +
  labs(title = "Estonia's Effectively Applied (AHS) Tariff",
       subtitle = "World, all products, weighted average (%), 1995-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","EST_AHS.pdf"), plot = graph_out, width = 18, height = 8, units = "cm")
rm(graph_out)

graph_out <- ggplot() +
  geom_line(data = bilateral_flows1, aes(x = year, y = `Exports to EU`, color = "Exports to EU (before 2004 enlargement)"), linewidth = 0.5) +
  geom_line(data = bilateral_flows1, aes(x = year, y = `Imports from EU`, color = "Imports from EU (before 2004 enlargement)"), linewidth = 0.5) +
  geom_line(data = bilateral_flows2, aes(x = year, y = `Exports to EU`, color = "Exports to EU (after 2004 enlargement)"), linewidth = 0.5) +
  geom_line(data = bilateral_flows2, aes(x = year, y = `Imports from EU`, color = "Imports from EU (after 2004 enlargement)"), linewidth = 0.5) +
  labs(color = NULL) +
  scale_x_continuous(breaks = seq(1992, 2020, by = 3)) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Exports to EU (before 2004 enlargement)" = "dodgerblue2",
                                "Imports from EU (before 2004 enlargement)" = "firebrick2",
                                "Exports to EU (after 2004 enlargement)" = "dodgerblue4",
                                "Imports from EU (after 2004 enlargement)" = "firebrick4")) +
  labs(title = "Bilateral Trade Flows Between Estonia and European Union",
       subtitle = "Trade flows in millions of current US dollars, 1992-2019") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","Bilateral_flows.pdf"), plot = graph_out, width = 24, height = 8, units = "cm")
rm(graph_out)

## Tables

# mtp_exp_table <- as_huxtable(mtp_exp_side3)
# mtp_exp_table[1,] <- c("Years", "Germany", "Finland", "United Kingdom", "Lithuania", "Latvia", "Netherlands", "Norway", "Russia", "Sweden", "United States") 
# mtp_exp_table <- mtp_exp_table %>%
#   set_align(row = 1, col = everywhere, "center") %>%
#   set_bold(row = 1,col = everywhere) %>%
#   set_bottom_border(row = 1, col = everywhere) %>%
#   set_top_border(row=1,col=everywhere) %>%
#   set_number_format(row = everywhere, col = 2:11, 2) %>%
#   set_number_format(row = everywhere, col = 1, NA) %>%
#   set_bottom_border(row=nrow(mtp_exp_table),col=everywhere) %>%
#   set_font_size(8) %>%
#   set_height(10)
# 
# split_tables <- split_across(mtp_exp_table, after = 15, headers = TRUE)
# mtp_exp_table1 <- split_tables[[1]] %>%
#   set_caption("Estonia's 10 main trading partners, export side, trade flows in millions of current US dollars, 1992-2005.") %>%
#   set_label("tab:A1")
# mtp_exp_table2 <- split_tables[[2]] %>%
#   set_caption("Estonia's 10 main trading partners, export side, trade flows in millions of current US dollars, 2006-2019.") %>%
#   set_label("tab:A2")
# 
# mtp_imp_table <- as_huxtable(mtp_imp_side3)
# mtp_imp_table[1,] <- c("Years", "China", "Germany", "Finland", "United Kingdom", "Lithuania", "Latvia", "Netherlands", "Poland", "Russia", "Sweden") 
# mtp_imp_table <- mtp_imp_table %>%
#   set_align(row = 1, col = everywhere, "center") %>%
#   set_bold(row = 1,col = everywhere) %>%
#   set_bottom_border(row = 1, col = everywhere) %>%
#   set_top_border(row=1,col=everywhere) %>%
#   set_number_format(row = everywhere, col = 2:11, 2) %>%
#   set_number_format(row = everywhere, col = 1, NA) %>%
#   set_bottom_border(row=nrow(mtp_imp_table),col=everywhere) %>%
#   set_font_size(8)
# 
# split_tables <- split_across(mtp_imp_table, after = 15, headers = TRUE)
# mtp_imp_table1 <- split_tables[[1]] %>%
#   set_caption("Estonia's 10 main trading partners, import side, trade flows in millions of current US dollars, 1992-2005.") %>%
#   set_label("tab:A3")
# mtp_imp_table2 <- split_tables[[2]] %>%
#   set_caption("Estonia's 10 main trading partners, import side, trade flows in millions of current US dollars, 2006-2019.") %>%
#   set_label("tab:A4")
# 
# # Export tables to latex
# cat(to_latex(mtp_exp_table1),file=here("output","tables","mtp_exp_table1.tex"))
# cat(to_latex(mtp_exp_table2),file=here("output","tables","mtp_exp_table2.tex"))
# cat(to_latex(mtp_imp_table1),file=here("output","tables","mtp_imp_table1.tex"))
# cat(to_latex(mtp_imp_table2),file=here("output","tables","mtp_imp_table2.tex"))
# 
# rm(mtp_exp_side3, mtp_exp_table, mtp_exp_table1, mtp_exp_table2, mtp_imp_side3, mtp_imp_table, mtp_imp_table1, mtp_imp_table2)
