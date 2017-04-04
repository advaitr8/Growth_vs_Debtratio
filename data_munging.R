rm(list = ls())
install.packages("broom", dependencies = T)
library(broom)
library(foreign)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("/Users/Advait/Desktop/New_School/Christian.paper/Code")
getwd()
list.files()
#Make a country vector for desired countries
country_vector <- c("aus", "aut", "bel", "can", "deu", "dnk", "esp", "fra", "gbr", "grc", "ita", "jpn", "nld","prt", "swe", "usa")

##Get the raw dataset
df1 <- read.csv("data_draft1.csv", header = T)
str(df1)

##Pick the necessary columns from the full dataset
df2 <- df1 %>%
  select(quart,
         quarter, 
         aus.b.gdp:usa.b.gdp, 
         aus.gr.gdpv:usa.gr.gdpv,
         aus.fsi:usa.fsi,
         aus.gr.lf:usa.gr.lf)
str(df2)

#Drop Finland from the dataset
df2 <- subset(df2, select = -c(fin.b.gdp, fin.gr.gdpv, 
                               fin.fsi,fin.gr.lf ))
str(df2)

#Create an interim for debt_ratio which can later be STACKED into one single column
df_debt_ratio <- df2 %>% 
  select(aus.b.gdp:usa.b.gdp)
df_debt_ratio <- stack(df_debt_ratio)
str(df_debt_ratio)

#Create id from factors
df_debt_ratio$c_id <- as.numeric(df_debt_ratio$ind)

#Create an interim for growth rates which can later be STACKED into one single column
df_growth_rate <- df2 %>%
  select(aus.gr.gdpv:usa.gr.gdpv)
df_growth_rate <- stack(df_growth_rate)

#Create an interim for financial stress which can later be STACKED into one single column
df_fin_stress <- df2 %>% 
  select(aus.fsi:usa.fsi)
df_fin_stress <- stack(df_fin_stress)

#Create an interim for labor force growth which can later be STACKED into one single column
df_labor <- df2 %>%
  select(aus.gr.lf:usa.gr.lf)
df_labor <- stack(df_labor)

#Creating quarters repeating 16 times for each country. 
#No clue if this motherfucker is right.
df1$quarter
quarter <- rep(df1$quarter, 16)
length(quarter)
quarter[2075:2080]

#Creating years repeating 16 times for each country
df1$quart
time <- rep(df1$quart,16)

#Combine Quarter, ID, GDP, FSI, LF into one Dataframe
df_comp <- data.frame(time = time,
                      quarter = quarter,
                      country_id = df_debt_ratio$c_id,
                      growth_rate = df_growth_rate$values,
                      debt_to_gdp = df_debt_ratio$values,
                      fin_stress = df_fin_stress$values,
                      labor = df_labor$values)
str(df_comp)
######################################################################
#Creating lags is required for complete df
library(data.table)

#Create df_temp to build lags
df_temp <- as.data.table(df_comp)
df_temp$growth_lag <- rep(0,2080)
df_temp$debt_lag <- rep(0,2080)

df_temp <- df_temp[, growth_lag:=c(NA, growth_rate[-.N]),
                   by = country_id]
df_temp <- df_temp[, debt_lag:=c(NA, debt_to_gdp[-.N]),
                   by = country_id]
str(df_temp)
summary(df_temp)
#foo is to check if the lag variable works (which apparently it does)
foo  <- df_temp %>%
  select(country_id,growth_rate, growth_lag,
         debt_to_gdp,debt_lag)

df_comp <- df_temp



df_comp <- df_comp[complete.cases(df_comp),]
str(df_comp)
#df_comp is the full data frame.
#Fuck yeah motherfucker, suck my balls R.
