rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
install.packages("lspline")
library(lspline)
# Estimate robust SE
install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
install.packages("texreg")
library(texreg)
# For different themes
install.packages(ggthemes)
library(ggthemes)

clean_file <- "D:/CEU/1 semester/coding_1_data-management-and-analysis_with_r/ECBS-5208-Coding-1-Business-Analytics/Class_9/data/clean/WDI_co2_clean.csv"
df <- read_csv(clean_file)  

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()  

summary( df )

df <- df %>% mutate( gdptot = gdppc*population )

#   Two competing models:
#     1) co2emission = alpha + beta * gdptot
#     2) co2emission = alpha + beta * gdppc


# 1) co2 - gdptot: level-level model without scaling
ggplot( df , aes(x = gdptot, y = co2emission)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Total GDP (2000 int. const. $, PPP )", y = "CO2 emission (years)") 

# You can change the scale for Total GDP for checking log-transformation
ggplot( df , aes(x = gdptot, y = co2emission)) +
  geom_point() +
  geom_smooth(method="loess") + 
  labs(x = "Total GDP (2000 int. const. $, PPP, ln scale )",y = "CO2 emission (years)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# You can change the scale for Total GDP and life-expectancy for checking log-transformation
ggplot( df , aes(x = gdptot, y = co2emission)) +
  geom_point() +
  geom_smooth(method="loess") + 
  labs(x = "Total GDP (2000 int. const. $, PPP, ln scale )",y = "CO2 emission (years, ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) ) +
  scale_y_continuous( trans = log_trans() )

# 2) lifeexp - gdppc: level-level model without scaling
ggplot( df , aes(x = gdppc, y = co2emission)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2000 int. const. $, PPP )",y = "CO2 emission  (years)") 

# Not looking good
# You can change the scale for GDP/capita for checking log-transformation
ggplot( df , aes(x = gdppc, y = co2emission)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2000 int. const. $, PPP , ln scale )",y = "CO2 emission (years)") +
  scale_x_continuous( trans = log_trans(), breaks = seq(0, 120, by = 20))


#Looking better: parabola, convex, reverted
# Ati: This has better than simple linear regression
# Cool: Ati had HTML Conclusion table like Agoston did last time
# You can change the scale for GDP/capita and life-expectancy for checking log-transformation
ggplot( df , aes(x = gdppc, y = co2emission ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2000 int. const. $, PPP , ln scale )",y = "CO2 emission (years, ln scale)") +
  scale_x_continuous( trans = log_trans(), breaks = seq(0, 120, by = 20))+
  scale_y_continuous( trans = log_trans() )
