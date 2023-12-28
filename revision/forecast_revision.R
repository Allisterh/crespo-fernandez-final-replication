
# Code to reproduce Table 1 in the paper #

# Clean working space and load necessary libraries #

rm(list=ls())

# Set a timer 

start.time <- Sys.time()
print(start.time)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, BMS, Cairo, reshape2, 
               ggplot2, writexl,stringr,lubridate)


# Load the necessary functions 

func.files <- list.files(path = "funcs", pattern = "\\.R$", full.names = TRUE)

for (file in func.files) {
  
  source(file)
  
}


# Set seed and number of iterations + burnin phase

set.seed(14091998)
n.iter = 500000
n.burn = 10000


# Read the dataset, set WD #

data_path = "data"

# Set dates 

end.fcast.date = as.Date("2013-10-01")
start.fcast.date = as.Date("2009-10-01")

# Read the data to import the synchronization variables 
# Note that any of the two files "synch_levels.xlsx" or "synch_synch.xlsx"
# Contain the same number of observations, we can choose any

fulldata = read_excel(file.path(data_path, "synch_levels.xlsx")) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date <= end.fcast.date, 
         !country %in% c("slovakia"))

####################################################################################################
#                                           AR (pooled) MODELS                                     #
####################################################################################################


# Subset the data to forecast and complete with NA for some countries are missing some dates
# Keep the observation for 2009Q4 because we will need it to forecast
# We have 17 observations per country: 16 to forecast + 1 the initial one 2009Q4

ar_forecast_data = fulldata %>% 
  filter(date >= start.fcast.date) %>% 
  select(country, synch,date)

ar_forecast_data = complete(ar_forecast_data, country, date) %>% 
  mutate(f_synch = NA)

# Create all the AR models #
# We need in total 16 models (we use up until the last_obs - 1 to forecast)


ar.pooled.1 = lm(synch ~ synch_lag1 + country + factor(year), data = fulldata %>% 
                   filter(date <= start.fcast.date)) 

for (jj in 2:16) {
  
  
  nam <- paste("ar.pooled.", jj, sep = "")
  assign(nam, lm(synch ~ synch_lag1 + country + factor(year), data = fulldata %>% 
                   filter(date <= start.fcast.date + months(3*jj))) )
  
  
}

# Now, create the forecasts #
# Note that the first observation is the last observation used in the models
# Hence, it is the first one used to build the forecast
# Add not only the intercept term but also all the country fixed effects
# Remember, we omit Austria (which acts as the intercept now)

ar_forecast_data = ar_forecast_data %>% 
  group_by(country) %>% 
  mutate(f_synch = case_when(
    row_number() == 2 ~ ar.pooled.1$coefficients[2]*synch[1] + sum(ar.pooled.1$coefficients[c(1,3:13)]),
    row_number() == 3 ~ ar.pooled.2$coefficients[2]*synch[2] + sum(ar.pooled.2$coefficients[c(1,3:13)]),
    row_number() == 4 ~ ar.pooled.3$coefficients[2]*synch[3] + sum(ar.pooled.3$coefficients[c(1,3:13)]),
    row_number() == 5 ~ ar.pooled.4$coefficients[2]*synch[4] + sum(ar.pooled.4$coefficients[c(1,3:13)]),
    row_number() == 6 ~ ar.pooled.5$coefficients[2]*synch[5] + sum(ar.pooled.5$coefficients[c(1,3:13)]),
    row_number() == 7 ~ ar.pooled.6$coefficients[2]*synch[6] + sum(ar.pooled.6$coefficients[c(1,3:13)]),
    row_number() == 8 ~ ar.pooled.7$coefficients[2]*synch[7] + sum(ar.pooled.7$coefficients[c(1,3:13)]),
    row_number() == 9 ~ ar.pooled.8$coefficients[2]*synch[8] + sum(ar.pooled.8$coefficients[c(1,3:13)]),
    row_number() == 10 ~ ar.pooled.9$coefficients[2]*synch[9] + sum(ar.pooled.9$coefficients[c(1,3:13)]),
    row_number() == 11 ~ ar.pooled.10$coefficients[2]*synch[10] + sum(ar.pooled.10$coefficients[c(1,3:13)]),
    row_number() == 12 ~ ar.pooled.11$coefficients[2]*synch[11] + sum(ar.pooled.11$coefficients[c(1,3:13)]),
    row_number() == 13 ~ ar.pooled.12$coefficients[2]*synch[12] + sum(ar.pooled.12$coefficients[c(1,3:13)]),
    row_number() == 14 ~ ar.pooled.13$coefficients[2]*synch[13] + sum(ar.pooled.13$coefficients[c(1,3:13)]),
    row_number() == 15 ~ ar.pooled.14$coefficients[2]*synch[14] + sum(ar.pooled.14$coefficients[c(1,3:13)]),
    row_number() == 16 ~ ar.pooled.15$coefficients[2]*synch[15] + sum(ar.pooled.15$coefficients[c(1,3:13)]),
    row_number() == 17 ~ ar.pooled.16$coefficients[2]*synch[16] + sum(ar.pooled.16$coefficients[c(1,3:13)])
  )
  )


# Plot the results 

plot.fcast(ar_forecast_data)

ggsave("plots/fcast_pooled.eps", width = 8, height = 6, units = "in", device = cairo_ps)

####################################################################################################
#                                           AR (country-specific) MODELS                           #
####################################################################################################

# First, get the data for each country
# Get the data from "ar_forecast_data_ (already expanded)

country.specific.data = fulldata %>% 
  select(date, country, synch, synch_lag1) %>% 
  complete(country, date) %>% 
  mutate(f_synch = NA)

# Create a data frame for each country

for(jj in unique(country.specific.data$country) ) { 
  nam <- paste(jj, ".data", sep = "")
  assign(nam, country.specific.data %>% 
           filter(country == jj))
}

# Now, run the AR model for each country and predict #
# Note that 2009-10-01 is in the 36th row
# They are all the same length, so choose last iteration arbitrarily
# Create list with all datasets

country.list = list(austria.data, 
                    belgium.data, 
                    finland.data, 
                    france.data, 
                    germany.data, 
                    greece.data, 
                    ireland.data, 
                    italy.data, 
                    netherlands.data, 
                    portugal.data, 
                    spain.data,
                    slovenia.data)

for (hh in 1:length(country.list)) {
  
  current.data = country.list[[hh]]
  
  for (kk in 36:(nrow(austria.data)-1)) {
    
    
    model = lm(synch~synch_lag1, data = current.data[1:kk,]) # Run the AR model
    current.data[kk+1,5] = model$coefficients[1] + model$coefficients[2] * current.data[kk,3] # Make the prediction
    
  }
  
  country.list[[hh]] = current.data
  
}

# Now, simply bind rows and subset the forecasting period

ar.country.data = bind_rows(country.list) %>% 
  select(-synch_lag1) %>% 
  filter(date >= start.fcast.date)

plot.fcast(ar.country.data) 
ggsave("plots/fcast_country.eps", width = 8, height = 6, units = "in", device = cairo_ps)


# ####################################################################################################
# #                                           BMA MODELS                                             #
# ####################################################################################################

# Create dummy names for the BMA exercise

year_dummy_names = c("d_2002","d_2003","d_2004","d_2005","d_2006","d_2007",
                     "d_2008","d_2009","d_2010","d_2011","d_2012","d_2013")

country_dummy_names = c("d_belgium","d_finland","d_france","d_germany","d_greece",
                        "d_ireland","d_italy","d_netherlands","d_portugal","d_spain")


# Read the data: remember, for BMA we need both synch-levels and synch-synch!
# Important: do not estimate the models with ZLB and Draghi dummies (neither the interactions)
# Note: the "real" dataset contains 95 independent variables = 6*4+(2-1)+6*2*4+22

# Synch-levels data

synch_levels_data = read_excel(file.path(data_path, "synch_levels.xlsx")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2013-10-01"), 
         !country %in% c("slovakia")) %>% 
  select(!contains("zlb"))

synch_levels_data = synch_levels_data %>%
  select(-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0)) %>%
  
  mutate(d_2002 = ifelse(year == 2002, 1 , 0),
         d_2003 = ifelse(year == 2003, 1 , 0),
         d_2004 = ifelse(year == 2004, 1 , 0),
         d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0)) %>%
  select(-year, -pigs)


# Synch-synch data

synch_synch_data = read_excel(file.path(data_path, "synch_synch.xlsx")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2013-10-01"), 
         !country %in% c("slovakia")) %>% 
  select(!contains("zlb")) 
 

synch_synch_data = synch_synch_data %>%
  select(-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0)) %>%
  
  mutate(d_2002 = ifelse(year == 2002, 1 , 0),
         d_2003 = ifelse(year == 2003, 1 , 0),
         d_2004 = ifelse(year == 2004, 1 , 0),
         d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0)) %>%
  select(-year, -pigs)



# Create the dataset for each model where we populate the forecasted quantities
# BMA synch-levels

bma.synch.levels.forecast.data = synch_levels_data %>%
  filter(date >= start.fcast.date)

bma.synch.levels.forecast.data = complete(bma.synch.levels.forecast.data, country, date) %>%
  mutate(f_synch = NA)

# BMA synch-synch

bma.synch.synch.forecast.data = synch_synch_data %>%
  filter(date >= start.fcast.date)

bma.synch.synch.forecast.data = complete(bma.synch.synch.forecast.data, country, date) %>%
  mutate(f_synch = NA)





# Create all the BMA models #
# Very important: consider the time fixed effects that correspond to each forecasting period
# E.g., if we are using 2001-2019 data, we cannot add 2020 fixed effect
# SYNCH-LEVELS (16 models) #

# Up to 2009 data

bma.model.synch.levels.1 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date) %>%
                                 select(-date, -country, -d_2010, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(9,10,11,12)], country_dummy_names))
# Up to 2010 data

bma.model.synch.levels.2 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(3)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))


bma.model.synch.levels.3 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(6)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

bma.model.synch.levels.4 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(9)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

bma.model.synch.levels.5 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(12)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

# Up to 2011 data


bma.model.synch.levels.6 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(15)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))

bma.model.synch.levels.7 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(18)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))


bma.model.synch.levels.8 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(21)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))


bma.model.synch.levels.9 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(24)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))

# Up to 2012 data


bma.model.synch.levels.10 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(27)) %>%
                                 select(-date, -country, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))

bma.model.synch.levels.11 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(30)) %>%
                                 select(-date, -country, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


bma.model.synch.levels.12 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(33)) %>%
                                 select(-date, -country, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


bma.model.synch.levels.13 = bms(synch_levels_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(36)) %>%
                                 select(-date, -country, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


# Up to 2013 data


bma.model.synch.levels.14 = bms(synch_levels_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(39)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))
bma.model.synch.levels.15 = bms(synch_levels_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(42)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))

bma.model.synch.levels.16 = bms(synch_levels_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(45)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))


# SYNCH-SYNCH (16 models) #

# Up to 2009 data

bma.model.synch.synch.1 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date) %>%
                                 select(-date, -country, -d_2010, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(9,10,11,12)], country_dummy_names))
# Up to 2010 data

bma.model.synch.synch.2 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(3)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))


bma.model.synch.synch.3 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(6)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

bma.model.synch.synch.4 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(9)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

bma.model.synch.synch.5 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(12)) %>%
                                 select(-date, -country, -d_2011, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(10,11,12)], country_dummy_names))

# Up to 2011 data


bma.model.synch.synch.6 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(15)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))

bma.model.synch.synch.7 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(18)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))


bma.model.synch.synch.8 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(21)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))


bma.model.synch.synch.9 = bms(synch_synch_data %>%
                                 mutate(date = as.Date(date)) %>%
                                 filter(date <= start.fcast.date + months(24)) %>%
                                 select(-date, -country, -d_2012, -d_2013),
                               burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                               nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                               fixed.reg = c(year_dummy_names[-c(11,12)], country_dummy_names))

# Up to 2012 data


bma.model.synch.synch.10 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(27)) %>%
                                  select(-date, -country, -d_2013),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))

bma.model.synch.synch.11 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(30)) %>%
                                  select(-date, -country, -d_2013),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


bma.model.synch.synch.12 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(33)) %>%
                                  select(-date, -country, -d_2013),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


bma.model.synch.synch.13 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(36)) %>%
                                  select(-date, -country, -d_2013),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names[-c(12)], country_dummy_names))


# Up to 2013 data


bma.model.synch.synch.14 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(39)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))
bma.model.synch.synch.15 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(42)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))

bma.model.synch.synch.16 = bms(synch_synch_data %>%
                                  mutate(date = as.Date(date)) %>%
                                  filter(date <= start.fcast.date + months(45)) %>%
                                  select(-date, -country),
                                burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random",
                                nmodel = 10000, mcmc = "bd", user.int = F, randomizeTimer = F, 
                                fixed.reg = c(year_dummy_names, country_dummy_names))




# Create the forecasts #
# Find indices of certain variables because not all used for prediction #

f_synch_index = which( colnames(bma.synch.levels.forecast.data)=="f_synch" ) # Get the column number of the preallocated forecast
country_index = which( colnames(bma.synch.levels.forecast.data)=="country" )
synch_index = which( colnames(bma.synch.levels.forecast.data)=="synch" )
date_index = which( colnames(bma.synch.levels.forecast.data)=="date" )

d_2009_index = which( colnames(bma.synch.levels.forecast.data)=="d_2009" )
d_2010_index = which( colnames(bma.synch.levels.forecast.data)=="d_2010" )
d_2011_index = which( colnames(bma.synch.levels.forecast.data)=="d_2011" )
d_2012_index = which( colnames(bma.synch.levels.forecast.data)=="d_2012" )
d_2013_index = which( colnames(bma.synch.levels.forecast.data)=="d_2013" )

# Arrange by country and date for consistency for both models 

bma.synch.levels.forecast.data = bma.synch.levels.forecast.data %>%
  arrange(country,date)

bma.synch.synch.forecast.data = bma.synch.synch.forecast.data %>% 
  arrange(country, date)

# Populate the forecasted values for synch-levels and synch-synch
# First sequence: seq(2,204,17)
# Second sequence: seq(3,204,17) and so on


len.seq = dim(bma.synch.levels.forecast.data)[1] / 
  length(unique(bma.synch.levels.forecast.data$country))

final.seq = dim(bma.synch.levels.forecast.data)[1]


# Populate the BMA datasets 

bma.synch.levels.forecast.data = populate.bma(bma.synch.levels.forecast.data, f_synch_index, country_index, synch_index, date_index, 
             d_2010_index, d_2011_index, d_2012_index, d_2013_index)

bma.synch.synch.forecast.data = populate.bma(bma.synch.synch.forecast.data, f_synch_index, country_index, synch_index, date_index, 
                                              d_2010_index, d_2011_index, d_2012_index, d_2013_index)

# Make the final plots 

plot.fcast(bma.synch.levels.forecast.data)
ggsave("plots/fcast_synch_levels.eps", width = 8, height = 6, units = "in", device = cairo_ps)

plot.fcast(bma.synch.synch.forecast.data)
ggsave("plots/fcast_synch_synch.eps", width = 8, height = 6, units = "in", device = cairo_ps)


# Code to check variables with PIP > 50%

synch.levels.list = list(bma.model.synch.levels.1, 
                         bma.model.synch.levels.2, 
                         bma.model.synch.levels.3, 
                         bma.model.synch.levels.4, 
                         bma.model.synch.levels.5, 
                         bma.model.synch.levels.6, 
                         bma.model.synch.levels.7, 
                         bma.model.synch.levels.8, 
                         bma.model.synch.levels.9, 
                         bma.model.synch.levels.10, 
                         bma.model.synch.levels.11, 
                         bma.model.synch.levels.12, 
                         bma.model.synch.levels.13, 
                         bma.model.synch.levels.14, 
                         bma.model.synch.levels.15, 
                         bma.model.synch.levels.16)


synch.synch.list = list(bma.model.synch.synch.1, 
                         bma.model.synch.synch.2, 
                         bma.model.synch.synch.3, 
                         bma.model.synch.synch.4, 
                         bma.model.synch.synch.5, 
                         bma.model.synch.synch.6, 
                         bma.model.synch.synch.7, 
                         bma.model.synch.synch.8, 
                         bma.model.synch.synch.9, 
                         bma.model.synch.synch.10, 
                         bma.model.synch.synch.11, 
                         bma.model.synch.synch.12, 
                         bma.model.synch.synch.13, 
                         bma.model.synch.synch.14, 
                         bma.model.synch.synch.15, 
                         bma.model.synch.synch.16)

cat("\014")  
for (catmodel in c("levels", "synch")) {
  
  for (jj in 1:16) {
    
    current.model = paste0("bma.model.synch", "." , catmodel, ".", jj)

    check.pip = as.data.frame(coef(eval(sym(current.model))))
  
  
  check.pip = check.pip %>%
    filter(PIP>=.5)
  
  
  check.pip = check.pip %>%
    filter(!(row.names(check.pip) %in% country_dummy_names),
           !(row.names(check.pip) %in% year_dummy_names))
  
  print(cat(sort(rownames(check.pip))))
  }
  
}


# Once we get the variables with PIP > 50%, now it's time to build the models with variables PIP > 50%

# SYNCH - LEVELS # 
synch_levels_data_pip = read_excel(file.path(data_path, "synch_levels.xlsx")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2013-10-01"), 
         !country %in% c("slovakia")) %>% 
  select(!contains("zlb"))

synch_levels_data_pip = synch_levels_data_pip %>%
  select(-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0)) %>%
  
  mutate(d_2002 = ifelse(year == 2002, 1 , 0),
         d_2003 = ifelse(year == 2003, 1 , 0),
         d_2004 = ifelse(year == 2004, 1 , 0),
         d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0)) %>%
  select(-year, -pigs)

# Create the dataset

synch_levels_pip_forecast_data = synch_levels_data_pip %>% 
  filter(date >= as.Date("2009-10-01")) 

synch_levels_pip_forecast_data = complete(synch_levels_pip_forecast_data, country, date) %>% 
  mutate(f_synch = NA)


# Create all the AR models #
# We need in total 16 models (we use up until the last_obs - 1 to forecast)

ar.synch.levels.pip50.1 = lm(synch ~ country + recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009, data = synch_levels_data_pip %>% 
                               filter(date <= "2009-10-01")) 

ar.synch.levels.pip50.2 = lm(synch ~  country  + pigsgdp_lag4 + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-01-01")) 

ar.synch.levels.pip50.3 = lm(synch ~  country  + pigsgdp_lag4 + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-04-01")) 

ar.synch.levels.pip50.4 = lm(synch ~  country  + pigsgdp_lag4 + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-07-01")) 

ar.synch.levels.pip50.5 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2010-10-01")))

ar.synch.levels.pip50.6 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-01-01")))

ar.synch.levels.pip50.7 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-04-01")))

ar.synch.levels.pip50.8 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-07-01")))

ar.synch.levels.pip50.9 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-10-01")))

ar.synch.levels.pip50.10 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2012-01-01")))

ar.synch.levels.pip50.11 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2012-04-01")))


ar.synch.levels.pip50.12 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                               recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_levels_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2012-07-01")))


ar.synch.levels.pip50.13 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                                recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_levels_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2012-10-01")))


ar.synch.levels.pip50.14 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                                recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_levels_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-01-01")))


ar.synch.levels.pip50.15 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                                recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_levels_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-04-01")))


ar.synch.levels.pip50.16 = lm(synch ~ country + pigsgdp_lag4 + recsynch_lag1 + recsynch_lag3 + 
                                recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_levels_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-07-01")))



# Generate now the predictions 


for (mm in 1:dim(synch_levels_pip_forecast_data)[1]) {
  
  if (mm %in% seq(2,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.1, newdata = synch_levels_pip_forecast_data[mm,c("recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:8])])
    
  }
  
  if (mm %in% seq(3,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.2, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(4,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.3, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(5,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.4, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(6,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.5, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(7,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.6, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
  }
  
  if (mm %in% seq(8,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.7, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
  }
  
  if (mm %in% seq(9,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.8, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
    
  }
  
  if (mm %in% seq(10,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.9, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
    
  }
  
  if (mm %in% seq(11,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.10, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(12,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.11, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(13,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.12, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(14,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.13, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(15,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.14, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
  if (mm %in% seq(16,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.15, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
  if (mm %in% seq(17,final.seq,len.seq)) {
    
    synch_levels_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.levels.pip50.16, newdata = synch_levels_pip_forecast_data[mm,c("pigsgdp_lag4", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
}


synch_levels_pip_forecast_data = synch_levels_pip_forecast_data %>% 
  select(date, country, f_synch)

plot.fcast(synch_levels_pip_forecast_data)
ggsave("plots/fcast_synch_levels_pip.eps", width = 8, height = 6, units = "in", device = cairo_ps)





# SYNCH - SYNCH # 

synch_synch_data_pip = read_excel(file.path(data_path, "synch_synch.xlsx")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date("2013-10-01"), 
         !country %in% c("slovakia")) %>% 
  select(!contains("zlb"))

synch_synch_data_pip = synch_synch_data_pip %>%
  select(-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0)) %>%
  
  mutate(d_2002 = ifelse(year == 2002, 1 , 0),
         d_2003 = ifelse(year == 2003, 1 , 0),
         d_2004 = ifelse(year == 2004, 1 , 0),
         d_2005 = ifelse(year == 2005, 1 , 0),
         d_2006 = ifelse(year == 2006, 1 , 0),
         d_2007 = ifelse(year == 2007, 1 , 0),
         d_2008 = ifelse(year == 2008, 1 , 0),
         d_2009 = ifelse(year == 2009, 1 , 0),
         d_2010 = ifelse(year == 2010, 1 , 0),
         d_2011 = ifelse(year == 2011, 1 , 0),
         d_2012 = ifelse(year == 2012, 1 , 0),
         d_2013 = ifelse(year == 2013, 1 , 0)) %>%
  select(-year, -pigs)

# Create the dataset

synch_synch_pip_forecast_data = synch_synch_data_pip %>% 
  filter(date >= as.Date("2009-10-01")) 

synch_synch_pip_forecast_data = complete(synch_synch_pip_forecast_data, country, date) %>% 
  mutate(f_synch = NA)


# Create all the AR models #
# We need in total 16 models (we use up until the last_obs - 1 to forecast)

ar.synch.synch.pip50.1 = lm(synch ~ country + recsynch_lag4 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009, data = synch_synch_data_pip %>% 
                               filter(date <= "2009-10-01")) 

ar.synch.synch.pip50.2 = lm(synch ~  country  + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-01-01")) 

ar.synch.synch.pip50.3 = lm(synch ~  country  + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-04-01")) 

ar.synch.synch.pip50.4 = lm(synch ~  country  + recsynch_lag1 + synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= "2010-07-01")) 

ar.synch.synch.pip50.5 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 + 
                              d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2010-10-01")))

ar.synch.synch.pip50.6 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 + 
                              d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-01-01")))

ar.synch.synch.pip50.7 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 +
                              d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                               d_2008+ d_2009+ d_2010 + d_2011, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-04-01")))

ar.synch.synch.pip50.8 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 +
                              d_2002 + d_2003 + d_2004 + d_2005 + d_2006 + d_2007 +
                               d_2008 + d_2009 + d_2010 + d_2011, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-07-01")))

ar.synch.synch.pip50.9 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 +
                              d_2002 + d_2003 + d_2004 + d_2005 + d_2006 + d_2007 +
                               d_2008 + d_2009 + d_2010 + d_2011, data = synch_synch_data_pip %>% 
                               mutate(date = as.Date(date)) %>% 
                               filter(date <= as.Date("2011-10-01")))

ar.synch.synch.pip50.10 = lm(synch ~ country + pigsdebttogdp_lag1+ pigsdebttogdp_lag2+ pigsgdp_lag1+ pigssynch_lag2+
                               recsynch_lag1 +recsynch_lag3 +recsynch_lag4 +synch_lag1 +
                               d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2012-01-01")))

ar.synch.synch.pip50.11 = lm(synch ~ country + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 + synch_lag1 + 
                               d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2012-04-01")))


ar.synch.synch.pip50.12 = lm(synch ~ country + pigsdebttogdp_lag4+ pigsgdp_lag1 + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 +
                               synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2012-07-01")))


ar.synch.synch.pip50.13 = lm(synch ~ country + pigsdebttogdp_lag3 + pigsgdp_lag1 + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 +
                               synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2012-10-01")))


ar.synch.synch.pip50.14 = lm(synch ~ country + pigsdebttogdp_lag3 + pigsgdp_lag1 + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 +
                               synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-01-01")))


ar.synch.synch.pip50.15 = lm(synch ~ country + pigsdebttogdp_lag3 + pigsgdp_lag1 + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 +
                               synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-04-01")))


ar.synch.synch.pip50.16 = lm(synch ~ country + pigsdebttogdp_lag3 + pigsgdp_lag1 + recsynch_lag1 + recsynch_lag3 + recsynch_lag4 +
                               synch_lag1 + d_2002+ d_2003+ d_2004+ d_2005+ d_2006+ d_2007+
                                d_2008+ d_2009+ d_2010 + d_2011 + d_2012 + d_2013, data = synch_synch_data_pip %>% 
                                mutate(date = as.Date(date)) %>% 
                                filter(date <= as.Date("2013-07-01")))



# Generate now the predictions 


for (mm in 1:dim(synch_synch_pip_forecast_data)[1]) {
  
  if (mm %in% seq(2,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.1, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:8])])
    
  }
  
  if (mm %in% seq(3,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.2, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(4,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.3, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(5,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.4, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(6,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.5, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:9])])
    
  }
  
  if (mm %in% seq(7,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.6, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
  }
  
  if (mm %in% seq(8,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.7, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
  }
  
  if (mm %in% seq(9,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.8, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
    
  }
  
  if (mm %in% seq(10,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.9, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:10])])
    
  }
  
  if (mm %in% seq(11,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.10, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag1", "pigsdebttogdp_lag2", "pigsgdp_lag1", "pigssynch_lag2", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(12,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.11, newdata = synch_synch_pip_forecast_data[mm,c("recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(13,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.12, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag4", "pigsgdp_lag1", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(14,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.13, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag3", "pigsgdp_lag1", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:11])])
    
  }
  
  if (mm %in% seq(15,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.14, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag3", "pigsgdp_lag1", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
  if (mm %in% seq(16,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.15, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag3", "pigsgdp_lag1", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
  if (mm %in% seq(17,final.seq,len.seq)) {
    
    synch_synch_pip_forecast_data[mm,f_synch_index] =
      predict(ar.synch.synch.pip50.16, newdata = synch_synch_pip_forecast_data[mm,c("pigsdebttogdp_lag3", "pigsgdp_lag1", "recsynch_lag1", "recsynch_lag3", "recsynch_lag4", "synch_lag1", "country", year_dummy_names[1:12])])
    
  }
  
}


synch_synch_pip_forecast_data = synch_synch_pip_forecast_data %>% 
  select(date, country, f_synch)

plot.fcast(synch_synch_pip_forecast_data)
ggsave("plots/fcast_synch_synch_pip.eps", width = 8, height = 6, units = "in", device = cairo_ps)


# End the timer

current.time = Sys.time()
difftime(current.time, start.time, units = 'mins')