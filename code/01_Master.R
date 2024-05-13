##############################################################
# Master file for the data repository of the RIA report
# Bruno Paese, Gellért Turveki-Nagy and Ebenezer Sosu
# March 2024
##############################################################


# Install packages (if necessary) and Load libraries --------------------------------

# source(here("code","01_PackageManagement.R"))
library(here) # easy file referencing, root is set at project root

# Setup - Define global variables -------------------------------------------------------------------

source(here("code","02_Setup.R"))

# Dataprep - Prepare the data ----------------------------------------------------------------

source(here("code","04_DataPrep.R"))

# Analysis ----------------------------------------------------------------

source(here("code","05_Analysis.R"))
source(here("code","06_TraditionalGravityEstimates.R"))
source(here("code","07_DistancePuzzle.R"))
source(here("code","08_FTAs.R"))
