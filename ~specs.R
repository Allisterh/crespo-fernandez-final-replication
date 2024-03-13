# File with the specifications to replicate the paper 

# String with the data path

data_path = "data"

# Load latex fonts

font_import(pattern = "lmroman*") 
loadfonts()

# Number of iterations and burn-in for BMA models 

n.burn.bma = 10000
n.iter.bma = 1e+07

# Number of iterations and burn-in for BMA models (forecast section)

n.burn.fcast = 10000
n.iter.fcast = 500000

# Width and height for jointness plots

width = 1600
height = 900

# Create year and country dummy names (important: only for BMA models)

year_dummy_names = c("year_2002",
                     "year_2003",
                     "year_2004",
                     "year_2005",
                     "year_2006",
                     "year_2007",
                     "year_2008",
                     "year_2009",
                     "year_2010",
                     "year_2011",
                     "year_2012",
                     "year_2013",
                     "year_2014",
                     "year_2015",
                     "year_2016",
                     "year_2017",
                     "year_2018",
                     "year_2019",
                     "year_2020", 
                     "year_2021")

country_dummy_names = c("country_belgium", 
                        "country_finland", 
                        "country_france", 
                        "country_germany", 
                        "country_greece", 
                        "country_ireland", 
                        "country_italy", 
                        "country_latvia", 
                        "country_lithuania", 
                        "country_netherlands", 
                        "country_portugal", 
                        "country_slovakia", 
                        "country_slovenia", 
                        "country_spain")


# Create PI(I)GS indicator variable to subset the forecasted measures #

piigs_subset = c("spain", "portugal", "ireland", "greece", "italy")

