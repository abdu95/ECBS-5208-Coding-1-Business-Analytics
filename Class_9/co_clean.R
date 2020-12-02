rm(list=ls())

library(tidyverse)
file_path <- "D:/CEU/1 semester/coding_1_data-management-and-analysis_with_r/ECBS-5208-Coding-1-Business-Analytics/Class_9/data/raw/WDI_co2_raw.csv"
df <- read_csv(file_path)

df <- df %>% filter( !grepl("[[:digit:]]", df$iso2c) )
drop_id <- c("EU","HK","OE")
# Check for filtering
df %>% filter( grepl( paste( drop_id , collapse="|"), df$iso2c ) ) 
# Save the opposite
df <- df %>% filter( !grepl( paste( drop_id , collapse="|"), df$iso2c ) )

fl_iso2c <- substr(df$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")
# Check
d1 <- df %>% filter( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                       !grepl( paste( retain_id , collapse="|"), df$iso2c ) ) 
# Save observations which are the opposite (use of !)
df <- df %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_id , collapse="|"), df$iso2c ) ) ) 

rm( d1 , drop_id, fl_iso2c , retain_id )

m <- df %>% filter( !complete.cases( df ) )
df <- df %>% filter( complete.cases( df ) | is.na( df$iso2c ) )

df <-df %>% transmute( country = country,
                       population=SP.POP.TOTL/1000000,
                       gdppc=NY.GDP.PCAP.KD/1000,
                       co2emission= EN.ATM.CO2E.PC)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )

path_to_save <- "D:/CEU/1 semester/coding_1_data-management-and-analysis_with_r/ECBS-5208-Coding-1-Business-Analytics/Class_9/data/"
write_csv( df, paste0(path_to_save,'clean/WDI_co2_clean.csv'))
