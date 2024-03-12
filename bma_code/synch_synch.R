
# Clean working space #

rm(list=ls())
base::set.seed(14091998)

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}

# Read the corresponding dataset 

fulldata = readxl::read_excel(file.path(data_path, "synch_synch.xlsx"))

# Add Draghi dummy + interactions

fulldata = add.draghi.synch(fulldata)

# Model with Fixed Effects #

# Select appropriate variables #
# Create dummy variables to control for year and country fixed effects #
# Note: need to create dummies except for one country and year group! #
# We have in total 15 countries and 21 years #
# So in total, 14 country dummies + 20 year dummies = 34 FE #
# All countries except Spain, all years except for 2021 #
# Important: to avoid multicollinearity issues, do not estimate dummy for PI(I)GS #

fe_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 
  
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dummy_cols(select_columns = "country", remove_first_dummy = T) %>% 
  dplyr::select(-year,-country, -pigs)

model_fe <-  BMS::bms(fe_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                 nmodel = 10000, mcmc = "bd", user.int = T, 
                 fixed.reg = c(year_dummy_names, country_dummy_names), randomizeTimer = F)


coefs_fe <- stats::coef(model_fe,  std.coefs = T, order.by.pip = F)

# Do not select the Fixed Effects results (year & country) #

coefs_fe = coefs_fe[!row.names(coefs_fe) %in% c(country_dummy_names, year_dummy_names),1:3]

# Before moving into the Fixed Effects model, run the jointness analysis #
# Compute jointness for all variables

jointness.dw2 = jointness.score(model_fe, method = "DW2")
jointness.ls2 = jointness.score(model_fe, method = "LS2")
jointness.yqm = jointness.score(model_fe, method = "YQM")


# Reorganize the columns of the matrix #

col.order = base::rownames(stats::coef(model_fe, order.by.pip = F))

jointness.dw2 = jointness.dw2[col.order,col.order]
jointness.ls2 = jointness.ls2[col.order,col.order]
jointness.yqm = jointness.yqm[col.order,col.order]





# Melt the jointness measures to discover the maximum values #

jointness.dw2 = reshape2::melt(jointness.dw2)
jointness.ls2 = reshape2::melt(jointness.ls2)
jointness.yqm = reshape2::melt(jointness.yqm)


# Convert to numeric to turn NaN and . into NA values # 

jointness.dw2 = jointness.dw2 %>% 
  dplyr::mutate(value = as.numeric(value))

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



# Reproduce Figure A.5 in the paper

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

# Reproduce Figure A.6 in the paper

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

# Reproduce Figure A.2 in the paper

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
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
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
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  select(-year,-country)

  
# Need to change interaction variable names #
# For consistency with BMS package # 

heredity_data = rename.synch(heredity_data)

model_heredity =  bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                      nmodel = 10000, mcmc = "bd.int", user.int = F, 
                      fixed.reg = year_dummy_names, randomizeTimer = F)


coefs_heredity <- coef(model_heredity,  std.coefs = T, order.by.pip = F)

# Do not select the year fixed effects #

coefs_heredity = coefs_heredity[!row.names(coefs_heredity) %in% year_dummy_names,1:3]


# Reproduce all columns of Table 2 in the paper 

round(coefs_fe, 4)
round(coefs_nofe, 4)
round(coefs_heredity, 4)
