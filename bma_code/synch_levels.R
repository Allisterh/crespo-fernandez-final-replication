
# Clean working space #

rm(list=ls())
base::set.seed(14091998)

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}

rm(file, func.files, n.burn, n.iter)

# Produce state-dependent synchronization tests (Table A.3 in the paper)

synch.data = readxl::read_excel(file.path(data_path, "synch_levels.xlsx")) %>% 
  dplyr::select(date, country, synch, rec, zlb) %>% 
  dplyr::mutate(draghi = dplyr::if_else(date >= '2012-07-01', 1, 0))

# Get unique country list 

country.list = base::sort(base::unique(synch.data$country))

# Create three arrays for the results (recession, ZLB, Draghi)

rec.results = base::as.data.frame(base::cbind(country.list, 
                                  base::array(0, c(length(country.list),1))), row.names = F) %>% 
  dplyr::rename(Country = country.list, Pval = V2)

# Note: for ZLB and Draghi is country.list-2 because Latvia and Lithuania was always either in ZLB or always in Draghi!

lv.lt.country.list = country.list[! country.list %in% c('latvia', 'lithuania')]

zlb.results = base::as.data.frame(base::cbind(lv.lt.country.list, 
                                  base::array(0, c(base::length(lv.lt.country.list),1))), row.names = F) %>% 
  dplyr::rename(Country = lv.lt.country.list, Pval = V2)

draghi.results = as.data.frame(base::cbind(lv.lt.country.list, 
                                     base::array(0, c(base::length(lv.lt.country.list),1))), row.names = F) %>% 
  dplyr::rename(Country = lv.lt.country.list, Pval = V2)


# T-test for recession variable

for (jj in 1:length(country.list)) {
  
  rec.results[jj,2] = base::round(my.ttest(country.list[jj], rec)$p.value,5)
  
  
}

rec.results = rec.results %>% 
  dplyr::mutate(Pval = base::as.numeric(Pval), 
         Significant = dplyr::if_else(Pval < 0.05, "Yes", "No"), 
         Country = stringr::str_to_title(Country))


# T-test for ZLB variable 

for (jj in 1:length(lv.lt.country.list)) {
  
  zlb.results[jj,2] = base::round(my.ttest(lv.lt.country.list[jj], zlb)$p.value,5)
  
  
}

zlb.results = zlb.results %>% 
  dplyr::mutate(Pval = base::as.numeric(Pval), 
         Significant = dplyr::if_else(Pval < 0.05, "Yes", "No"), 
         Country = stringr::str_to_title(Country))


# T-test for Draghi variable 

for (jj in 1:length(lv.lt.country.list)) {
  
  draghi.results[jj,2] = base::round(my.ttest(lv.lt.country.list[jj], draghi)$p.value,5)
  
  
}

draghi.results = draghi.results %>% 
  dplyr::mutate(Pval = base::as.numeric(Pval), 
         Significant = dplyr::if_else(Pval < 0.05, "Yes", "No"), 
         Country = stringr::str_to_title(Country))

# Print the tables (Table A.3 in the paper)

print(rec.results, row.names = F) 
print(zlb.results, row.names = F) 
print(draghi.results, row.names = F) 

# Start here BMS models 

# Clean working space

rm(list=ls())

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}

# Read the corresponding dataset

fulldata = readxl::read_excel(base::file.path(data_path, "synch_levels.xlsx"))

# Reproduce synchronization figure in the paper (Figure 3 in the paper)

fulldata %>% 
  dplyr::mutate(country = stringr::str_to_title(country)) %>% 
  ggplot2::ggplot(aes(x = date, y = synch)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  ggplot2::facet_wrap(~country, scales = "free", nrow = 3) +
  ggplot2::labs(x = "Date", y = "Long-term yield synchronization rates")


# Data availability by country (Table A.1 in the paper)

fulldata %>% 
  dplyr::select(country, date) %>% 
  dplyr::mutate(country = stringr::str_to_title(country)) %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(`Start Date` = as.Date(min(date)), `End Date` = as.Date(max(date))) %>% 
  dplyr::ungroup()


# Summary statistics by country (Table A.2 in the paper, select specific country to display)
# Get the summary statistics for each country in the sample

for (mycountry in sort(unique(fulldata$country))) {
  
  base::print(mycountry)
  stats.country(fulldata, mycountry)
  
  
}


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

model_fe <- fit.bms(fe_data, 1)


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
  dplyr::mutate(value = as.numeric(value))

jointness.yqm = jointness.yqm %>% 
  dplyr::mutate(value = as.numeric(value))

# Get the maximum values in order to select the variables
# that present the higher jointness

max.jointness.dw2 = jointness.dw2 %>% 
  dplyr::filter(value == base::max(value, na.rm = T))

max.jointness.ls2 = jointness.ls2 %>% 
  dplyr::filter(value == base::max(value, na.rm = T))

max.jointness.yqm = jointness.yqm %>% 
  dplyr::filter(value == base::max(value, na.rm = T))


# In order to get unique values, consider creating a pair variable
# and checking the unique values
# Also, do not display the year nor country fixed effects variables

max.jointness.dw2 = max.jointness.dw2 %>% 
  dplyr::mutate(pair = base::paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  dplyr::distinct(pair) %>% 
  dplyr::filter(!grepl('year_',pair)) %>% 
  dplyr::filter(!grepl('country_',pair))



max.jointness.ls2 = max.jointness.ls2 %>% 
  dplyr::mutate(pair = base::paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  dplyr::distinct(pair) %>% 
  dplyr::filter(!grepl('year_',pair)) %>% 
  dplyr::filter(!grepl('country_',pair))

max.jointness.yqm = max.jointness.yqm %>% 
  dplyr::mutate(pair = base::paste(pmin(as.character(Var1), as.character(Var2)), 
                      pmax(as.character(Var1), as.character(Var2)), sep = '-')) %>% 
  dplyr::distinct(pair) %>% 
  dplyr::filter(!grepl('year_',pair)) %>% 
  dplyr::filter(!grepl('country_',pair))

# Plot the matrices #
# Recall: we do not show the year fixed effects #




# Reproduce Figure A.1 in the paper

jointness.yqm %>% 
  dplyr::filter(!grepl("year_", Var1)) %>% 
  dplyr::filter(!grepl("year_", Var2)) %>% 
  dplyr::filter(!grepl("country_", Var1)) %>% 
  dplyr::filter(!grepl("country_", Var2)) %>% 
  ggplot2::ggplot(aes(Var1, Var2, fill= value)) + 
  ggplot2::geom_tile() + 
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggplot2::labs(x = "", y = "") + 
  ggplot2::theme(axis.text=element_text(size=6)) +  
  ggplot2::scale_fill_continuous( low = "black", high = "red") +
  ggplot2::theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  ggplot2::guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))

# Reproduce Figure A.3 in the paper

jointness.dw2 %>% 
  dplyr::filter(!grepl("year_", Var1)) %>% 
  dplyr::filter(!grepl("year_", Var2)) %>% 
  dplyr::filter(!grepl("country_", Var1)) %>% 
  dplyr::filter(!grepl("country_", Var2)) %>% 
  ggplot2::ggplot(aes(Var1, Var2, fill= value)) + 
  ggplot2::geom_tile() + 
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggplot2::labs(x = "", y = "") + 
  ggplot2::theme(axis.text=element_text(size=5)) +  
  ggplot2::scale_fill_continuous( low = "black", high = "red") +
  ggplot2::theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  ggplot2::guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))

# Reproduce Figure A.4 in the paper

jointness.ls2 %>% 
  dplyr::filter(!grepl("year_", Var1)) %>% 
  dplyr::filter(!grepl("year_", Var2)) %>% 
  dplyr::filter(!grepl("country_", Var1)) %>% 
  dplyr::filter(!grepl("country_", Var2)) %>% 
  ggplot2::ggplot(aes(Var1, Var2, fill= value)) + 
  ggplot2::geom_tile() + 
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggplot2::labs(x = "", y = "") + 
  ggplot2::theme(axis.text=element_text(size=5)) +  
  ggplot2::scale_fill_continuous( low = "black", high = "red") +
  ggplot2::theme(legend.text = element_text(size=7), legend.title = element_blank()) + 
  ggplot2::guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20))



# Model without Fixed Effects: add only time (year) fixed effects  #

nofe_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>%
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dplyr::select(-year,-country)



model_nofe <- BMS::bms(nofe_data, burn = n.burn, iter = n.iter,  g = "BRIC", mprior = "random", 
                  nmodel = 10000, mcmc = "bd", user.int = F,  
                  fixed.reg = year_dummy_names, randomizeTimer = F)


coefs_nofe <- stats::coef(model_nofe,  std.coefs = T, order.by.pip = F)[,1:3]

# Do not select year fixed effects #

coefs_nofe = coefs_nofe[!row.names(coefs_nofe) %in% c(year_dummy_names),1:3]

# Model with strong heredity prior: add only time (year) fixed effects #
# Note that this is the same dataset used for the no Fixed Effects model #

heredity_data = fulldata %>% 
  dplyr::select(-date,-uncert,-bop,-debttogdp,-gdp,-euribor,-inflation) %>% 
  dummy_cols(select_columns = "year", remove_first_dummy = T) %>%
  dplyr::select(-year,-country)

  
# Need to change interaction variable names #
# For consistency with BMS package # 

heredity_data = rename.synch(heredity_data)

model_heredity =  BMS::bms(heredity_data, burn = n.burn, iter = n.iter, g = "BRIC", mprior = "random", 
                      nmodel = 10000, mcmc = "bd.int", user.int = F, 
                      fixed.reg = year_dummy_names, randomizeTimer = F)


coefs_heredity <- stats::coef(model_heredity,  std.coefs = T, order.by.pip = F)

# Do not select the year fixed effects #

coefs_heredity = coefs_heredity[!row.names(coefs_heredity) %in% year_dummy_names,1:3]

# Reproduce all columns of Table 1 in the paper 

base::round(coefs_fe, 4)
base::round(coefs_nofe, 4)
base::round(coefs_heredity, 4)
