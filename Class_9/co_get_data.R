# Clear memory
rm(list=ls())

library(readr)
install.packages('WDI')
library(WDI)

# How WDI works - it is an API

gdp_data <- WDI(indicator='NY.GDP.PCAP.KD', country="all", start=2000, end=2000)

#indicator is kind of ID
#iso2c - country ID

# Get all the data - 2018 is the latest available data for life expectancy
data_raw <- WDI(indicator=c('NY.GDP.PCAP.KD','EN.ATM.CO2E.PC', 'SP.POP.TOTL'), 
                country="all", start=2000, end=2000)


# Save the raw data file
my_path <- "D:/CEU/1 semester/coding_1_data-management-and-analysis_with_r/ECBS-5208-Coding-1-Business-Analytics/Class_9/data/"
write_csv(data_raw, paste0(my_path,'raw/WDI_co2_raw.csv'))
