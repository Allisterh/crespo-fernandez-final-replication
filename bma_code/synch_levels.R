
# Clean working space and load necessary libraries #

rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(BMS)
library(Cairo)
library(reshape2)
library(ggplot2)
library(writexl)
library(xtable)
library(stargazer)

# Set seed and number of iterations + burnin phase

set.seed(14091998)
n.burn = 10000
n.iter = 1e7

# Read the dataset, set WD and read jointness function #

source("jointness_function.R")
data_path = "../bma_data"

fulldata = read_excel(file.path(data_path, "synch_levels.xlsx"))


# Reproduce synchronization figure in the paper (Figure 3)

fulldata %>% 
  mutate(country = case_when(
    country == "austria" ~ "Austria", 
    country == "belgium" ~ "Belgium", 
    country == "finland" ~ "Finland", 
    country == "france" ~ "France", 
    country == "germany" ~ "Germany", 
    country == "greece" ~ "Greece", 
    country == "ireland" ~ "Ireland", 
    country == "italy" ~ "Italy", 
    country == "latvia" ~ "Latvia", 
    country == "lithuania" ~ "Lithuania", 
    country == "netherlands" ~ "Netherlands", 
    country == "portugal" ~ "Portugal", 
    country == "slovakia" ~ "Slovakia", 
    country == "slovenia" ~ "Slovenia", 
    country == "spain" ~ "Spain",
    TRUE~country
  )) %>% 
  ggplot(aes(x = date, y = synch)) + 
  geom_line() + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  facet_wrap(~country, scales = "free", nrow = 3) +
  labs(x = "Date", y = "Long-term yield synchronization rates")


# Data availability by country (Table A.1)

fulldata %>% 
  select(country, date) %>% 
  mutate(country = case_when(
  country == "austria" ~ "Austria", 
  country == "belgium" ~ "Belgium", 
  country == "finland" ~ "Finland", 
  country == "france" ~ "France", 
  country == "germany" ~ "Germany", 
  country == "greece" ~ "Greece", 
  country == "ireland" ~ "Ireland", 
  country == "italy" ~ "Italy", 
  country == "latvia" ~ "Latvia", 
  country == "lithuania" ~ "Lithuania", 
  country == "netherlands" ~ "Netherlands", 
  country == "portugal" ~ "Portugal", 
  country == "slovakia" ~ "Slovakia", 
  country == "slovenia" ~ "Slovenia", 
  country == "spain" ~ "Spain",
  TRUE~country)) %>% 
  group_by(country) %>% 
  summarise(`Start Date` = as.Date(min(date)), `End Date` = as.Date(max(date))) 


# Summary statistics by country (Table A.2, select specific country to display)

my_func = function(my_country) {  
  
  sum.stats = fulldata %>% 
  filter(country == as.name(my_country)) %>% 
  summarize(round(mean(synch), 3), round(sd(synch), 3), round(min(synch), 3), round(max(synch), 3),
            round(mean(uncert), 3), round(sd(uncert), 3), round(min(uncert), 3), round(max(uncert), 3), 
            round(mean(gdp), 3), round(sd(gdp), 3), round(min(gdp), 3), round(max(gdp), 3), 
            round(mean(debttogdp), 3), round(sd(debttogdp), 3), round(min(debttogdp), 3), round(max(debttogdp), 3), 
            round(mean(inflation), 3), round(sd(inflation), 3), round(min(inflation), 3), round(max(inflation), 3), 
            round(mean(rec), 3), round(sd(rec), 3), round(min(rec), 3), round(max(rec), 3))
  
    print(sum.stats[1:4])
    print(sum.stats[5:8])
    print(sum.stats[9:12])
    print(sum.stats[13:16])
    print(sum.stats[17:20])
    print(sum.stats[21:24])
  
}

# Example below for Austria

my_func("austria")



# Add Draghi dummy + interactions

fulldata = fulldata %>% 
  mutate(draghi = ifelse(date >= '2012-07-01', 1, 0), 
         draghisynch_lag1 = draghi * synch_lag1, 
         draghisynch_lag2 = draghi * synch_lag2, 
         draghisynch_lag3 = draghi * synch_lag3, 
         draghisynch_lag4 = draghi * synch_lag4, 
         draghiuncert_lag1 = draghi * uncert_lag1,
         draghiuncert_lag2 = draghi * uncert_lag2,
         draghiuncert_lag3 = draghi * uncert_lag3,
         draghiuncert_lag4 = draghi * uncert_lag4, 
         draghigdp_lag1 = draghi * gdp_lag1, 
         draghigdp_lag2 = draghi * gdp_lag2,
         draghigdp_lag3 = draghi * gdp_lag3,
         draghigdp_lag4 = draghi * gdp_lag4, 
         draghibop_lag1 = draghi * bop_lag1, 
         draghibop_lag2 = draghi * bop_lag2,
         draghibop_lag3 = draghi * bop_lag3,
         draghibop_lag4 = draghi * bop_lag4, 
         draghidebttogdp_lag1 = draghi * debttogdp_lag1, 
         draghidebttogdp_lag2 = draghi * debttogdp_lag2, 
         draghidebttogdp_lag3 = draghi * debttogdp_lag3, 
         draghidebttogdp_lag4 = draghi * debttogdp_lag4, 
         draghiinflation_lag1 = draghi * inflation_lag1, 
         draghiinflation_lag2 = draghi * inflation_lag2,
         draghiinflation_lag3 = draghi * inflation_lag3,
         draghiinflation_lag4 = draghi * inflation_lag4) 





# Create dummy names


year_dummy_names = c("d_2002",
                     "d_2003",
                     "d_2004",
                     "d_2005",
                     "d_2006",
                     "d_2007",
                     "d_2008",
                     "d_2009",
                     "d_2010",
                     "d_2011",
                     "d_2012",
                     "d_2013",
                     "d_2014",
                     "d_2015",
                     "d_2016",
                     "d_2017",
                     "d_2018",
                     "d_2019",
                     "d_2020", 
                     "d_2021")

country_dummy_names = c("d_belgium", 
                        "d_finland", 
                        "d_france", 
                        "d_germany", 
                        "d_greece", 
                        "d_ireland", 
                        "d_italy", 
                        "d_latvia", 
                        "d_lithuania", 
                        "d_netherlands", 
                        "d_portugal", 
                        "d_slovakia", 
                        "d_slovenia", 
                        "d_spain")

# Model with Fixed Effects #

# Select appropriate variables #
# Create dummy variables to control for year and country fixed effects #
# Note: need to create dummies except for one country and year group! #
# We have in total 15 countries and 21 years #
# So in total, 14 country dummies + 20 year dummies = 34 FE #
# All countries except Spain, all years except for 2021 #
# Important: to avoid multicollinearity issues, do not estimate dummy for PI(I)GS #


fe_data = fulldata %>% 
  select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 
  
  mutate(d_belgium = ifelse(country == "belgium", 1 , 0),
         d_finland = ifelse(country == "finland", 1 , 0),
         d_france = ifelse(country == "france", 1 , 0),
         d_germany = ifelse(country == "germany", 1 , 0),
         d_greece = ifelse(country == "greece", 1 , 0),
         d_ireland = ifelse(country == "ireland", 1 , 0),
         d_italy = ifelse(country == "italy", 1 , 0),
         d_latvia = ifelse(country == "latvia", 1 , 0),
         d_lithuania = ifelse(country == "lithuania", 1, 0),
         d_netherlands = ifelse(country == "netherlands", 1 , 0),
         d_portugal = ifelse(country == "portugal", 1 , 0),
         d_slovakia = ifelse(country == "slovakia", 1 , 0),
         d_slovenia = ifelse(country == "slovenia", 1 , 0),
         d_spain = ifelse(country == "spain", 1 , 0)) %>%

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
         d_2013 = ifelse(year == 2013, 1 , 0),
         d_2014 = ifelse(year == 2014, 1 , 0),
         d_2015 = ifelse(year == 2015, 1 , 0),
         d_2016 = ifelse(year == 2016, 1 , 0),
         d_2017 = ifelse(year == 2017, 1 , 0),
         d_2018 = ifelse(year == 2018, 1 , 0),
         d_2019 = ifelse(year == 2019, 1 , 0),
         d_2020 = ifelse(year == 2020, 1 , 0),
         d_2021 = ifelse(year == 2021, 1 , 0)) %>% 
  select(-year,-country, -pigs)

model_fe <-  bms(fe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = F, 
                 fixed.reg = c(year_dummy_names, country_dummy_names), randomizeTimer = F)


coefs_fe <- coef(model_fe,  std.coefs = T, order.by.pip = F)

# Do not select the Fixed Effects results (year & country) #

coefs_fe = coefs_fe[!row.names(coefs_fe) %in% c(country_dummy_names, year_dummy_names),1:3]

# Before moving into the Fixed Effects model, run the jointness analysis #
# Compute jointness for all variables

jointness.dw2 = jointness.score(model_fe, method = "DW2")
jointness.ls2 = jointness.score(model_fe, method = "LS2")
jointness.yqm = jointness.score(model_fe, method = "YQM")


# Reorganize the columns of the matrix #

col.order = rownames(coef(model_fe, order.by.pip = F))

jointness.dw2 = jointness.dw2[col.order,col.order]
jointness.ls2 = jointness.ls2[col.order,col.order]
jointness.yqm = jointness.yqm[col.order,col.order]





# Melt the jointness measures to discover the maximum values #

jointness.dw2 = melt(jointness.dw2)
jointness.ls2 = melt(jointness.ls2)
jointness.yqm = melt(jointness.yqm)


# Convert to numeric to turn NaN and . into NA values # 

jointness.dw2 = jointness.dw2 %>% 
  mutate(value = as.numeric(value))

jointness.ls2 = jointness.ls2 %>% 
  mutate(value = as.numeric(value))

jointness.yqm = jointness.yqm %>% 
  mutate(value = as.numeric(value))

# Get the maximum values in order to select the variables
# that present the higher jointness

max.jointness.dw2 = jointness.dw2 %>% 
  filter(value == max(value, na.rm = T))

max.jointness.ls2 = jointness.ls2 %>% 
  filter(value == max(value, na.rm = T))

max.jointness.yqm = jointness.yqm %>% 
  filter(value == max(value, na.rm = T))


# In order to get unique values, consider creating a pair variable
# and checking the unique values
# Also, do not display the year nor country fixed effects variables

max.jointness.dw2 = max.jointness.dw2 %>% 
  mutate(pair = paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  distinct(pair) %>% 
  filter(!grepl('d_',pair))


max.jointness.ls2 = max.jointness.ls2 %>% 
  mutate(pair = paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  distinct(pair) %>% 
  filter(!grepl('d_',pair))


max.jointness.yqm = max.jointness.yqm %>% 
  mutate(pair = paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  distinct(pair) %>% 
  filter(!grepl('d_',pair))


# Plot the matrices #
# Recall: we do not show the year fixed effects #
width = 1600
height = 900



jointness.dw2 %>% 
  filter(!grepl("d_", Var1)) %>% 
  filter(!grepl("d_", Var2)) %>% 
  ggplot(aes(Var1, Var2, fill= value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "") + 
  theme(axis.text=element_text(size=5)) +  
  scale_fill_continuous( low = "black", high = "red") +
  theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))

jointness.ls2 %>% 
  filter(!grepl("d_", Var1)) %>% 
  filter(!grepl("d_", Var2)) %>% 
  ggplot(aes(Var1, Var2, fill= value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "") + 
  theme(axis.text=element_text(size=5)) +  
  scale_fill_continuous( low = "black", high = "red") +
  theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))

jointness.yqm %>% 
  filter(!grepl("d_", Var1)) %>% 
  filter(!grepl("d_", Var2)) %>% 
  ggplot(aes(Var1, Var2, fill= value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "") + 
  theme(axis.text=element_text(size=6)) +  
  scale_fill_continuous( low = "black", high = "red") +
  theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))

# Model without Fixed Effects: add only time (year) fixed effects  #

nofe_data = fulldata %>% 
  select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  
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
         d_2013 = ifelse(year == 2013, 1 , 0),
         d_2014 = ifelse(year == 2014, 1 , 0),
         d_2015 = ifelse(year == 2015, 1 , 0),
         d_2016 = ifelse(year == 2016, 1 , 0),
         d_2017 = ifelse(year == 2017, 1 , 0),
         d_2018 = ifelse(year == 2018, 1 , 0),
         d_2019 = ifelse(year == 2019, 1 , 0),
         d_2020 = ifelse(year == 2020, 1 , 0),
         d_2021 = ifelse(year == 2021, 1 , 0)) %>% 
  
  select(-year,-country)



model_nofe <- bms(nofe_data, burn = n.burn, iter = n.iter,  g = "BRIC", mprior = "random", 
                  nmodel = 10000, mcmc = "bd", user.int = F,  
                  fixed.reg = year_dummy_names, randomizeTimer = F)


coefs_nofe <- coef(model_nofe,  std.coefs = T, order.by.pip = F)[,1:3]

# Do not select year fixed effects #

coefs_nofe = coefs_nofe[!row.names(coefs_nofe) %in% c(year_dummy_names),1:3]

# Model with strong heredity prior: add only time (year) fixed effects #
# Note that this is the same dataset used for the no Fixed Effects model #

heredity_data = fulldata %>% 
  select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 

  
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
         d_2013 = ifelse(year == 2013, 1 , 0),
         d_2014 = ifelse(year == 2014, 1 , 0),
         d_2015 = ifelse(year == 2015, 1 , 0),
         d_2016 = ifelse(year == 2016, 1 , 0),
         d_2017 = ifelse(year == 2017, 1 , 0),
         d_2018 = ifelse(year == 2018, 1 , 0),
         d_2019 = ifelse(year == 2019, 1 , 0),
         d_2020 = ifelse(year == 2020, 1 , 0),
         d_2021 = ifelse(year == 2021, 1 , 0)) %>% 
  select(-year,-country)

  
# Need to change interaction variable names #
# For consistency with BMS package # 

heredity_data = heredity_data %>% 
  rename("rec#synch_lag1" = recsynch_lag1,
         "rec#synch_lag2" = recsynch_lag2,
         "rec#synch_lag3" = recsynch_lag3,
         "rec#synch_lag4" = recsynch_lag4,
         "rec#uncert_lag1" = recuncert_lag1,
         "rec#uncert_lag2" = recuncert_lag2,
         "rec#uncert_lag3" = recuncert_lag3,
         "rec#uncert_lag4" = recuncert_lag4,
         "rec#bop_lag1" = recbop_lag1,
         "rec#bop_lag2" = recbop_lag2,
         "rec#bop_lag3" = recbop_lag3,
         "rec#bop_lag4" = recbop_lag4, 
         "rec#debttogdp_lag1" = recdebttogdp_lag1,
         "rec#debttogdp_lag2" = recdebttogdp_lag2,
         "rec#debttogdp_lag3" = recdebttogdp_lag3,
         "rec#debttogdp_lag4" = recdebttogdp_lag4,
         "rec#gdp_lag1" = recgdp_lag1,
         "rec#gdp_lag2" = recgdp_lag2,
         "rec#gdp_lag3" = recgdp_lag3,
         "rec#gdp_lag4" = recgdp_lag4,
         "rec#inflation_lag1" = recinflation_lag1,
         "rec#inflation_lag2" = recinflation_lag2,
         "rec#inflation_lag3" = recinflation_lag3,
         "rec#inflation_lag4" = recinflation_lag4,
         "pigs#synch_lag1" = pigssynch_lag1,
         "pigs#synch_lag2" = pigssynch_lag2,
         "pigs#synch_lag3" = pigssynch_lag3,
         "pigs#synch_lag4" = pigssynch_lag4,
         "pigs#uncert_lag1" = pigsuncert_lag1,
         "pigs#uncert_lag2" = pigsuncert_lag2,
         "pigs#uncert_lag3" = pigsuncert_lag3,
         "pigs#uncert_lag4" = pigsuncert_lag4,
         "pigs#bop_lag1" = pigsbop_lag1,
         "pigs#bop_lag2" = pigsbop_lag2,
         "pigs#bop_lag3" = pigsbop_lag3,
         "pigs#bop_lag4" = pigsbop_lag4,
         "pigs#debttogdp_lag1" = pigsdebttogdp_lag1,
         "pigs#debttogdp_lag2" = pigsdebttogdp_lag2,
         "pigs#debttogdp_lag3" = pigsdebttogdp_lag3,
         "pigs#debttogdp_lag4" = pigsdebttogdp_lag4,
         "pigs#gdp_lag1" = pigsgdp_lag1,
         "pigs#gdp_lag2" = pigsgdp_lag2,
         "pigs#gdp_lag3" = pigsgdp_lag3,
         "pigs#gdp_lag4" = pigsgdp_lag4,
         "pigs#inflation_lag1" = pigsinflation_lag1,
         "pigs#inflation_lag2" = pigsinflation_lag2,
         "pigs#inflation_lag3" = pigsinflation_lag3,
         "pigs#inflation_lag4" = pigsinflation_lag4,
         "zlb#synch_lag1" = zlbsynch_lag1,
         "zlb#synch_lag2" = zlbsynch_lag2,
         "zlb#synch_lag3" = zlbsynch_lag3,
         "zlb#synch_lag4" = zlbsynch_lag4,
         "zlb#uncert_lag1" = zlbuncert_lag1,
         "zlb#uncert_lag2" = zlbuncert_lag2,
         "zlb#uncert_lag3" = zlbuncert_lag3,
         "zlb#uncert_lag4" = zlbuncert_lag4,
         "zlb#bop_lag1" = zlbbop_lag1,
         "zlb#bop_lag2" = zlbbop_lag2,
         "zlb#bop_lag3" = zlbbop_lag3,
         "zlb#bop_lag4" = zlbbop_lag4,
         "zlb#debttogdp_lag1" = zlbdebttogdp_lag1,
         "zlb#debttogdp_lag2" = zlbdebttogdp_lag2,
         "zlb#debttogdp_lag3" = zlbdebttogdp_lag3,
         "zlb#debttogdp_lag4" = zlbdebttogdp_lag4,
         "zlb#gdp_lag1" = zlbgdp_lag1,
         "zlb#gdp_lag2" = zlbgdp_lag2,
         "zlb#gdp_lag3" = zlbgdp_lag3,
         "zlb#gdp_lag4" = zlbgdp_lag4,
         "zlb#inflation_lag1" = zlbinflation_lag1,
         "zlb#inflation_lag2" = zlbinflation_lag2,
         "zlb#inflation_lag3" = zlbinflation_lag3,
         "zlb#inflation_lag4" = zlbinflation_lag4,
         
         "draghi#synch_lag1" = draghisynch_lag1,
         "draghi#synch_lag2" = draghisynch_lag2,
         "draghi#synch_lag3" = draghisynch_lag3,
         "draghi#synch_lag4" = draghisynch_lag4,
         "draghi#uncert_lag1" = draghiuncert_lag1,
         "draghi#uncert_lag2" = draghiuncert_lag2,
         "draghi#uncert_lag3" = draghiuncert_lag3,
         "draghi#uncert_lag4" = draghiuncert_lag4,
         "draghi#bop_lag1" = draghibop_lag1,
         "draghi#bop_lag2" = draghibop_lag2,
         "draghi#bop_lag3" = draghibop_lag3,
         "draghi#bop_lag4" = draghibop_lag4,
         "draghi#debttogdp_lag1" = draghidebttogdp_lag1,
         "draghi#debttogdp_lag2" = draghidebttogdp_lag2,
         "draghi#debttogdp_lag3" = draghidebttogdp_lag3,
         "draghi#debttogdp_lag4" = draghidebttogdp_lag4,
         "draghi#gdp_lag1" = draghigdp_lag1,
         "draghi#gdp_lag2" = draghigdp_lag2,
         "draghi#gdp_lag3" = draghigdp_lag3,
         "draghi#gdp_lag4" = draghigdp_lag4,
         "draghi#inflation_lag1" = draghiinflation_lag1,
         "draghi#inflation_lag2" = draghiinflation_lag2,
         "draghi#inflation_lag3" = draghiinflation_lag3,
         "draghi#inflation_lag4" = draghiinflation_lag4)


model_heredity =  bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                      nmodel = 10000, mcmc = "bd.int", user.int = F, 
                      fixed.reg = year_dummy_names, randomizeTimer = F)


coefs_heredity <- coef(model_heredity,  std.coefs = T, order.by.pip = F)

# Do not select the year fixed effects #

coefs_heredity = coefs_heredity[!row.names(coefs_heredity) %in% year_dummy_names,1:3]


