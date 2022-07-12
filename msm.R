library(MMAsurvival)
library(dplyr)
library(OHplot)
library(ggplot2)
theme_set(OH_style())

# Load Data

# IPD for Pemetrexd + Cisplatin
PC <- read_data("C:\\Users\\MKnowles\\OneDrive - OPEN Health\\Documents\\Test Project\\Clean\\IPDLung3PC.csv")
PC$treatment <- "PC"
# IPD for Afatinib
AF <- read_data("C:\\Users\\MKnowles\\OneDrive - OPEN Health\\Documents\\Test Project\\Clean\\IPDLung3AF.csv")
AF$treatment <- "AF"

# Combine Data
DMSM <- rbind(PC,AF)

# Define list of transitions
transitions <- list(
  list(from = "pre-progress", to ="progress", distribution = "exp"),
  list(from = "pre-progress", to ="deat", distribution = "exp"),
  list(from = "progress", to ="death", distribution = "weibull")
)

# Each outcome is linked to a set of states
outcomes <- list(
  OS = c("pre-progress", "progress"),
  PFS = "pre-progress"
)

# Assume everyone is healthy to begin with

init_states <- c(`pre-progress` = 1)

MSM(TTE(DF), transitions, outcomes, initial_states)
