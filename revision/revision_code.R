# Code to generate results for the revision

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(DescTools, tidyr, dplyr, readxl, ggplot2, lubridate, zoo, stringr,
               xtable, stargazer, stats, Hmisc, plm)

# Load the necessary functions 

func.files <- list.files(path = "funcs", pattern = "\\.R$", full.names = TRUE)

for (file in func.files) {
  
  source(file)
  
}



# REVIEWER 1, COMMENT 5: Redo the forecasting exercise without the Covid period # 

# Load the forecast results previously run (note that the previous results of Table 3 still hold)

rm(list = setdiff(ls(), lsf.str())) 
load("data/forecast_theil_all.RData")
load("data/forecast_theil_synch_levels.RData")
load("data/forecast_theil_synch_synch.RData")


# Collect the data for all models without 2020
# Pooled AR(1) model

model.1 = only.2020(ar_forecast_data)
model.1.piigs = only.2020(ar_forecast_data.piigs)
model.1.nopiigs = only.2020(ar_forecast_data.nopiigs)

# Country-specific AR(1) model

model.2 = only.2020(ar.country.data)
model.2.piigs = only.2020(ar.country.data.piigs)
model.2.nopiigs = only.2020(ar.country.data.nopiigs)

# BMA (levels)

model.3 = only.2020(bma.synch.levels.forecast.data)
model.3.piigs = only.2020(bma.synch.levels.forecast.data.piigs)
model.3.nopiigs = only.2020(bma.synch.levels.forecast.data.nopiigs)

# BMA (synch)

model.4 = only.2020(bma.synch.synch.forecast.data)
model.4.piigs = only.2020(bma.synch.synch.forecast.data.piigs)
model.4.nopiigs = only.2020(bma.synch.synch.forecast.data.nopiigs)

# BMA (levels, PIP > 50%)

model.5 = only.2020(synch_levels_pip_forecast_data)
model.5.piigs = only.2020(synch_levels_pip_forecast_data.piigs)
model.5.nopiigs = only.2020(synch_levels_pip_forecast_data.nopiigs)

# BMA (synch, PIP > 50%)

model.6 = only.2020(synch_synch_pip_forecast_data)
model.6.piigs = only.2020(synch_synch_pip_forecast_data.piigs)
model.6.nopiigs = only.2020(synch_synch_pip_forecast_data.nopiigs)


# Create a list of the models

# With all countries 

models.all <- list(model.1, model.2, model.3, model.4, model.5, model.6)

# With only PIIGS countries 

models.piigs <- list(model.1.piigs, model.2.piigs, model.3.piigs, model.4.piigs, 
                     model.5.piigs, model.6.piigs)

# With no PIIGS countries

models.nopiigs <- list(model.1.nopiigs, model.2.nopiigs, model.3.nopiigs, model.4.nopiigs, 
                     model.5.nopiigs, model.6.nopiigs)

# Loop over the models and call dir.results for each one

for (model in models.all) {
  
  cat(dir.results(model), "\n")
  
}

for (model in models.piigs) {
  
  cat(dir.results(model), "\n")
  
}

for (model in models.nopiigs) {
  
  cat(dir.results(model), "\n")
  
}



# REVIEWER 1, COMMENT 6: Add the Theil U statistic to the forecasting exercise # 

# Generate Table 3 again adding the Theil U statistics 

# Collect all the data in models again
# Pooled AR(1) model

model.1 = ar_forecast_data
model.1.piigs = ar_forecast_data.piigs
model.1.nopiigs = ar_forecast_data.nopiigs

# Country-specific AR(1) model

model.2 = ar.country.data
model.2.piigs = ar.country.data.piigs
model.2.nopiigs = ar.country.data.nopiigs

# BMA (levels)

model.3 = bma.synch.levels.forecast.data
model.3.piigs = bma.synch.levels.forecast.data.piigs
model.3.nopiigs = bma.synch.levels.forecast.data.nopiigs

# BMA (synch)

model.4 = bma.synch.synch.forecast.data
model.4.piigs = bma.synch.synch.forecast.data.piigs
model.4.nopiigs = bma.synch.synch.forecast.data.nopiigs

# BMA (levels, PIP > 50%)

model.5 = synch_levels_pip_forecast_data
model.5.piigs = synch_levels_pip_forecast_data.piigs
model.5.nopiigs = synch_levels_pip_forecast_data.nopiigs

# BMA (synch, PIP > 50%)

model.6 = synch_synch_pip_forecast_data
model.6.piigs = synch_synch_pip_forecast_data.piigs
model.6.nopiigs = synch_synch_pip_forecast_data.nopiigs


# Create a list of the models

# With all countries 

models.all <- list(model.1, model.2, model.3, model.4, model.5, model.6)

# With only PIIGS countries 

models.piigs <- list(model.1.piigs, model.2.piigs, model.3.piigs, model.4.piigs, 
                     model.5.piigs, model.6.piigs)

# With no PIIGS countries

models.nopiigs <- list(model.1.nopiigs, model.2.nopiigs, model.3.nopiigs, model.4.nopiigs, 
                       model.5.nopiigs, model.6.nopiigs)

# Loop over the models and call dir.results for each one

for (model in models.all) {
  
  cat(dir.results(model), "\n")
  
}

for (model in models.piigs) {
  
  cat(dir.results(model), "\n")
  
}

for (model in models.nopiigs) {
  
  cat(dir.results(model), "\n")
  
}

# REVIEWER 2, MINOR COMMENT 1: State-dependent synch

rm(list = setdiff(ls(), lsf.str())) 

synch.data = read_excel("data/synch_levels.xlsx") %>% 
  select(date, country, synch, rec, zlb) %>% 
  mutate(draghi = ifelse(date >= '2012-07-01', 1, 0))

# Get unique country list 

country.list = sort(unique(synch.data$country))

# Create three arrays for the results (recession, ZLB, Draghi)

rec.results = as.data.frame(cbind(country.list, 
                                  array(0, c(length(country.list),1))), row.names = F) %>% 
  rename(Country = country.list, Pval = V2)

# Note: for ZLB and Draghi is country.list-2 because Latvia and Lithuania was always either in ZLB or always in Draghi!

lv.lt.country.list = country.list[! country.list %in% c('latvia', 'lithuania')]

zlb.results = as.data.frame(cbind(lv.lt.country.list, 
                                  array(0, c(length(lv.lt.country.list),1))), row.names = F) %>% 
  rename(Country = lv.lt.country.list, Pval = V2)

draghi.results = as.data.frame(cbind(lv.lt.country.list, 
                                  array(0, c(length(lv.lt.country.list),1))), row.names = F) %>% 
  rename(Country = lv.lt.country.list, Pval = V2)


# T-test for recession variable

for (jj in 1:length(country.list)) {
  
  rec.results[jj,2] = round(my.ttest(country.list[jj], rec)$p.value,5)

  
}

rec.results = rec.results %>% 
  mutate(Pval = as.numeric(Pval), 
         Significant = ifelse(Pval < 0.05, "Yes", "No"), 
         Country = str_to_title(Country))


# T-test for ZLB variable 

for (jj in 1:length(lv.lt.country.list)) {
  
  zlb.results[jj,2] = round(my.ttest(lv.lt.country.list[jj], zlb)$p.value,5)
  
  
}

zlb.results = zlb.results %>% 
  mutate(Pval = as.numeric(Pval), 
         Significant = ifelse(Pval < 0.05, "Yes", "No"), 
         Country = str_to_title(Country))


# T-test for Draghi variable 

for (jj in 1:length(lv.lt.country.list)) {
  
  draghi.results[jj,2] = round(my.ttest(lv.lt.country.list[jj], draghi)$p.value,5)
  
  
}

draghi.results = draghi.results %>% 
  mutate(Pval = as.numeric(Pval), 
         Significant = ifelse(Pval < 0.05, "Yes", "No"), 
         Country = str_to_title(Country))

# Print the tables 

print(rec.results, row.names = F) 
print(zlb.results, row.names = F) 
print(draghi.results, row.names = F) 


# REVIEWER 2, COMMENT 1 d): residuals check 

rm(list = setdiff(ls(), lsf.str())) 

# Load the previously run results 

load("data/synch_levels_results.RData")

fe.predictions = predict(model_fe)
nofe.predictions = predict(model_nofe)
heredity.predictions = predict(model_heredity)

# Quick check 

table(fulldata$synch == fe_data$synch)
table(fulldata$synch == nofe_data$synch)
table(fulldata$synch == heredity_data$synch)


data.residuals = fulldata %>% 
  select(synch, country, date) %>% 
  mutate(fe_pred = predict(model_fe), 
         fe_resids = synch - fe_pred, 
         
         
         nofe_pred = predict(model_nofe), 
         nofe_resids = synch - nofe_pred, 
         
         
         heredity_pred = predict(model_heredity), 
         heredity_resids = synch - heredity_pred) %>% 
  select(-contains("pred"))


# Plot all results for all the models

countries = unique(fulldata$country)

# Model with fixed effects 

par(mfrow=c(3,5), mar=c(1,1,4,1))

for (country in countries) {
  
  plot.acf(country, "fe_resids")
  
}


# Model without fixed effects

par(mfrow=c(3,5), mar=c(1,1,4,1))

for (country in countries) {
  
  plot.acf(country, "nofe_resids")
  
}

# Heredity model 

par(mfrow=c(3,5), mar=c(1,1,4,1))

for (country in countries) {
  
  plot.acf(country, "heredity_resids")
  
}

# Compute residuals correlations 

# Create function to plot the p-values of the correlations test 

par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))


# Plot the results

pval.corrs.plot("fe_resids")
pval.corrs.plot("nofe_resids")
pval.corrs.plot("heredity_resids")

# Test of cross-sectional dependence

resids.pdata = data.residuals %>% 
  select(date, country, contains("resids")) %>% 
  arrange(country,date) %>% 
  pdata.frame()

fe.csd = pcdtest(resids.pdata$fe_resids, test = "cd")
nofe.csd = pcdtest(resids.pdata$nofe_resids, test = "cd")
heredity.csd = pcdtest(resids.pdata$heredity_resids, test = "cd")


c(round(as.numeric(fe.csd$statistic),4), round(as.numeric(fe.csd$p.value),4))
c(round(as.numeric(nofe.csd$statistic),4), round(as.numeric(nofe.csd$p.value),4))
c(round(as.numeric(heredity.csd$statistic),4), round(as.numeric(heredity.csd$p.value),4))
