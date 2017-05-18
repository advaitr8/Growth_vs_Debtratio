#rm(list = ls())
setwd("/Users/Advait/Desktop/New_School/Christian.paper/Growth_vs_Debtratio/Attempt3")

source("data_munging.R")
str(df_comp)
#######################################################################
#Exploratory data analysis
#######################################################################
str(df_comp)

#creating a financial stress id variable
length(df_comp$fin_stress)
df_comp$fs_id <- rep(NA, times = 1905)
for(i in 1:1930){
  if (df_comp$fin_stress[i] > -7 & df_comp$fin_stress[i] < -3){
    df_comp$fs_id[i] = 1}
  else if (df_comp$fin_stress[i] >= -3 & df_comp$fin_stress[i] < 0){
    df_comp$fs_id[i] = 2}
  else if (df_comp$fin_stress[i] >= 0 & df_comp$fin_stress[i] < 2){
    df_comp$fs_id[i] = 3}
  else if (df_comp$fin_stress[i] >= 2 & df_comp$fin_stress[i] < 4){
    df_comp$fs_id[i] = 4}
  else if (df_comp$fin_stress[i] >= 4 & df_comp$fin_stress[i] < 7){
    df_comp$fs_id[i] = 5}
  else if (df_comp$fin_stress[i] >= 7 & df_comp$fin_stress[i] < 18){
    df_comp$fs_id[i] = 6}
}
#The financial stress id is created here

###########################
#However the trend in ATTEMPT 1 appears to be because most of the data is concentrated at data points which correspond to financial stress levels at 2 and 3. Since this is where the most data is, the posteriors appear to be less variant here.
###########################

####Check in which finanancial stress bracket the most data points are
length_fs <- NULL
for(i in 1:6){
  length_fs[i] = length(df_comp$fs_id[df_comp$fs_id == i])
}
length_fs

##########
#######################################################################
#Stan models
#######################################################################
growth <- df_comp$growth_rate
growth_lag <- df_comp$growth_lag
debt_lag <- df_comp$debt_lag
labor <- df_comp$labor
fin_stress <- df_comp$fin_stress
country_id <- df_comp$country_id
t_id <- df_comp$t_id
#a little hack here for good plots
for(i in 1:1905){
  if(t_id[i] == 33){
    t_id[i] = 32}
}
#I make all time period 33 into time period 32
exch <- df_comp$exch
interest <- df_comp$interest
inf_rate <- df_comp$inf_rate
fs_id <- df_comp$fs_id

#######################################################################
#Notes to understand the regression
#######################################################################
# #The dependent variable is growth rate of GDP in the current period
# #The predictors are:
# growth lagged (1 period)
# debt lagged (1 period)
# labor force growth rate
# financial stress index
# exchange rate
# interest rate
# inflation rate
# 
# #categorical variables indicating:
# country
# time
# financial stress level



