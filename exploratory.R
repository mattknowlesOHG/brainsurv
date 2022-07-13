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
DF <- rbind(PC,AF)

# Check summaries to compare to paper

checkPaperQuants(PC,"PFS")
checkPaperQuants(PC,"OS")
checkPaperQuants(AF, "PFS")
checkPaperQuants(AF, "OS")

# Plotting

TTE(DF) %>% 
  plot(facet.by = "outcome", color.by = "treatment", xlab = "Time (Months)", ylab = "Survival Probability", conf.int = TRUE ,logrank.test = TRUE)

# Hazard ratio

hazard <- TTE(DF) %>% 
  hazard_ratio(covariates = "treatment", foreach = "outcome") %>% 
    format_doubles(decimals = 4)

create_table(hazard, groups = "outcome", type = "flextable")