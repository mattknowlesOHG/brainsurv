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
TTE(DF)

# Schoenfeld Residuals

TTE(DF) %>% 
  filter(outcome == "OS") %>% 
  plot_schoenfeld_residuals(covariates = c("treatment"), color = OH_pink, show.test.statistics = TRUE)

TTE(DF) %>% 
  filter(outcome == "PFS") %>% 
  plot_schoenfeld_residuals(covariates = c("treatment"), color = OH_pink, show.test.statistics = TRUE)









#Declare list of distributions
dists <- list(
  "Exponential"       = "exp",
  "Gompertz"          = "gompertz",
  "Weibull"           = "weibull",
  "Log-normal"        = "lnorm",
  "Log-logistic"      = "llogis",
  "Gamma"             = "gamma",
  "Generalized gamma" = "gengamma")

# Fit distributions

independent_models <- TTE(DF) %>%
  fit_distribution(distributions = dists, foreach = c("outcome", "treatment")) 

independent_models_coef <- coef(independent_models)
head(independent_models_coef)

#Plot models

independent_model_plot <- independent_models %>%
  plot(time = 120, color.by = "treatment", 
       facet.by = "Distribution", 
       foreach = "outcome", 
       linetype.observed = "dashed",
       linetype.model = "solid")

independent_model_plot$OS

# Singular Plot

plot_independent_models <- independent_models %>%
  filter(treatment == "PC") %>%
  plot(time = 120, foreach = "outcome", 
       color.by = "Distribution", color.observed = "black", 
       linetype.observed = "dashed", linetype.model = "solid", 
       xlab = "Time (months)")

plot_independent_models$OS
plot_independent_models$PFS


# Test closeness of a fit

independent_models %>%
  filter(treatment == "AF", 
         Distribution == "Generalized gamma", 
         outcome == "OS" | outcome == "PFS") %>%
  plot(time = 120, color.by = "outcome", linetype.model = "solid", linetype.observed = "dashed")


# Model selection

model_summary <- summary(independent_models) %>% 
  format_doubles()

model_summary %>% 
  filter(outcome == "OS") %>% 
  select(-outcome)

create_table(model_summary, groups = "outcome", type = "flextable")
