rm(list = ls())
# Preliminaries
library(car)
library(tidyverse)
library(ggplot2)
library(countrycode) #optional
library(gridExtra)
library(ggthemes)
 # set your own working directory

# Load data as of Aug 2021
owid_data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
summary(owid_data)

# Removing aggregated indicators (e.g. "Africa", "Europe", etc.)
data <- owid_data[!(is.na(owid_data$iso_code) | owid_data$iso_code == ""), ]

# Subset country level indicators
data <- subset(data, country != "World")
data <- data[, c(1,2,3,4,18,19,20,21,22,54,55)]

# Income classification as of Aug 2021
# "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519"
low_income <- c("Afghanistan", "Burkina Faso", "Burundi", "Central African Republic",
                "Chad", "Democratic Republic of Congo", "Eritrea", "Ethiopia",
                "Gambia", "Guinea", "Guinea-Bissau", "North Korea", "Liberia",
                "Madagascar", "Malawi", "Mali", "Mozambique", "Niger", "Rwanda",
                "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Syria",
                "Togo", "Uganda", "Yemen")
lowmid_income <- c("Angola", "Algeria", "Bangladesh", "Belize", "Benin", "Bhutan",
                   "Bolivia", "Cape Verde", "Cambodia", "Cameroon", "Comoros",
                   "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "El Salvador",
                   "Eswatini", "Ghana", "Haiti", "Honduras", "India", "Indonesia",
                   "Iran", "Kenya", "Kiribati", "Kyrgyzstan", "Laos", "Lesotho",
                   "Mauritania", "Micronesia", "Mongolia", "Morocco", "Myanmar",
                   "Nepal", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea",
                   "Philippines", "Samoa", "Sao Tome and Principe", "Senegal",
                   "Solomon Islands", "Sri Lanka", "Tanzania", "Tajikistan",
                   "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu",
                   "Vietnam", "West Bank and Gaza", "Zambia", "Zimbabwe")

uppmid_income <- c("Albania", "American Samoa", "Argentina", "Armenia", "Azerbaijan",
                   "Belarus", "Bosnia and Herzegovina", "Botswana", "Brazil", 
                   "Bulgaria", "China", "Colombia", "Costa Rica", "Cuba", "Dominica",
                   "Dominican Republic", "Equatorial Guinea", "Ecuador", "Fiji",
                   "Gabon", "Georgia", "Grenada", "Guatemala", "Guyana", "Iraq", 
                   "Jamaica", "Jordan", "Kazakhstan", "Kosovo", "Lebanon", "Libya",
                   "Malaysia", "Maldives", "Marshall Islands", "Mauritius", "Mexico",
                   "Moldova", "Montenegro", "Namibia", "North Macedonia", "Panama",
                   "Paraguay", "Peru", "Romania", "Russia", "Serbia", "South Africa",
                   "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname",
                   "Thailand", "Tonga", "Turkey", "Turkmenistan", "Tuvalu")
high_income <- c("Andorra", "Antigua and Barbuda", "Aruba", "Australia", "Austria",
                 "Bahamas", "Bahrain", "Barbados", "Belgium", "Bermuda",
                 "British Virgin Islands", "Brunei", "Canada", "Cayman Islands",
                 "Channel Islands", "Chile", "Croatia", "Curacao", "Cyprus",
                 "Czechia", "Denmark", "Estonia", "Faeroe Islands", "Finland",
                 "France", "French Polynesia", "Germany", "Gibraltar", "Greece",
                 "Greenland", "Guam", "Hong Kong", "Hungary", "Iceland", "Ireland",
                 "Isle of Man", "Israel", "Italy", "Japan", "South Korea", "Kuwait",
                 "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
                 "Nauru", "Netherlands", "New Caledonia", "New Zealand", "Norway",
                 "Oman", "Palau", "Poland", "Portugal", "Puerto Rico", "Qatar",
                 "San Marino", "Saudi Arabia", "Seychelles", "Singapore", "Monaco",
                 "Sint Maarten (Dutch part)", "Slovakia", "Slovenia", "Spain",
                 "Saint Kitts and Nevis", "St. Martin (French part)", "Sweden",
                 "Switzerland", "Taiwan", "Trinidad and Tobago", "Macao", "Gibraltar",
                 "Turks and Caicos Islands", "United Arab Emirates", "United Kingdom",
                 "United States", "Uruguay", "Virgin Islands (US)")

data <- data %>%
  mutate(D_low = as.integer(country %in% low_income), 
         D_lowmid = as.integer(country %in% lowmid_income),
         D_uppmid = as.integer(country %in% uppmid_income),
         D_high = as.integer(country %in% high_income))

data$co2 <- data$co2/1000
data$population <- data$population/1000000000
data$gdp <- data$gdp/1000000000000
#newvars /1000 /10000000 etc
data1 <- subset(data, year >= 1900)

# Part 3 (modeling)

mod1 <- lm(co2 ~ population + gdp , data = data1)
mod2 <- lm(co2 ~ population + gdp, data = subset(data1, D_high == 1))
mod3 <- lm(co2 ~ population + gdp, data = subset(data1, D_uppmid == 1))
mod4 <- lm(co2 ~ population + gdp, data = subset(data1, D_lowmid == 1))
mod5 <- lm(co2 ~ population + gdp, data = subset(data1, D_low == 1))
mod6<- lm(co2 ~ population + D_high +D_uppmid + D_lowmid+D_low - 1, data = data1)
mod7<- lm(co2 ~ population + gdp + D_high +D_uppmid + D_lowmid+D_low - 1, data = data1)
library(modelsummary)
model_list <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
modelsummary(model_list, stars = TRUE) # results
#more at github.com/solowiew

