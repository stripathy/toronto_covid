# install toronto open data

install.packages("opendatatoronto")

library(opendatatoronto)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)


neighbourhood_profiles = read.csv(url('https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv'))


neighbourhood_profiles %>% filter(Characteristic == 'Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)') 


avg_income_vector = neighbourhood_profiles %>% filter(X_id == 1030 | Characteristic == 'Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)')

avg_incomes_only = avg_income_vector[, 6:ncol(avg_income_vector)] 

avg_incomes_transposed = avg_incomes_only %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(var = "norm_neighbourhood_name")
colnames(avg_incomes_transposed) = c("norm_neighbourhood_name", 'avg_income', 'low_income_pct')
avg_incomes_transposed$avg_income = readr::parse_number(as.character(avg_incomes_transposed$avg_income))
avg_incomes_transposed$low_income_pct = as.numeric(as.character(avg_incomes_transposed$low_income_pct))


covid_data = read_excel('~/Downloads/CityofToronto_COVID-19_NeighbourhoodData.xlsx', sheet = 'All Cases and Rates by Neighbou')
covid_data$norm_neighbourhood_name = covid_data$`Neighbourhood Name` %>% make.names(unique = T)

merged_covid_data = merge(avg_incomes_transposed, covid_data, by = "norm_neighbourhood_name")

library(cowplot)
theme_set(theme_cowplot())

merged_covid_data %>% ggplot(aes(x = avg_income, y = `Rate per 100,000 people`)) + 
  geom_point() + scale_x_log10(name="Average after-tax income of households in 2015 ($)", labels = scales::comma) + 
  ylab("Covid rate per 100,000 people") + geom_smooth(method = "lm", se = F)

merged_covid_data %>% ggplot(aes(x = low_income_pct, y = `Rate per 100,000 people`)) + 
  geom_point() + scale_x_log10(name="Prevalence of low income based on the Low-income cut-offs,\n after tax (LICO-AT) (%)", labels = scales::comma) + 
  ylab("Covid rate per 100,000 people") + geom_smooth(method = "lm", se = F)


	