
# Clean working space #

rm(list=ls())

# Load the necessary functions, packages, and specifications

func.files <- base::list.files(path = "./", pattern = "^~", full.names = TRUE)

for (file in func.files) {
  
  base::source(file)
  
}


# Produce state-dependent synchronization tests (Table A.3 in the paper)

synch.data = readxl::read_excel(file.path(data_path, "synch_levels.xlsx")) %>% 
  #dplyr::select(date, country, synch, rec, zlb) %>% 
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


# Reproduce synchronization figure in the paper (Figure 3 in the paper)

synch.data %>% 
  dplyr::mutate(country = stringr::str_to_title(country)) %>% 
  ggplot2::ggplot(aes(x = date, y = synch)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  ggplot2::facet_wrap(~country, scales = "free", nrow = 3) +
  ggplot2::labs(x = "Date", y = "Long-term yield synchronization rates")

# Data availability by country (Table A.1 in the paper)

synch.data %>% 
  dplyr::select(country, date) %>% 
  dplyr::mutate(country = stringr::str_to_title(country)) %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(`Start Date` = as.Date(min(date)), `End Date` = as.Date(max(date))) %>% 
  dplyr::ungroup()


# Summary statistics by country (Table A.2 in the paper, select specific country to display)
# Get the summary statistics for each country in the sample

for (mycountry in sort(unique(synch.data$country))) {
  
  base::print(mycountry)
  stats.country(synch.data, mycountry)
  
  
}
