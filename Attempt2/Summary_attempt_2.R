#rm(list = ls())
setwd("/Users/Advait/Desktop/New_School/Christian.paper/Growth_vs_Debtratio/Attempt2")

source("data_munging.R")
str(df_comp)


#creating a financial stress id variable
length(df_comp$fin_stress)
df_comp$fs_id <- rep(NA, times = 1905)
for(i in 1:1905){
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
  else if (df_comp$fin_stress[i] >= 7 & df_comp$fin_stress[i] < 10){
    df_comp$fs_id[i] = 6}
  else if (df_comp$fin_stress[i] >= 10 & df_comp$fin_stress[i] < 14){
    df_comp$fs_id[i] = 7}
  else if (df_comp$fin_stress[i] >= 14 & df_comp$fin_stress[i] < 18){
    df_comp$fs_id[i] = 8}
}
#The financial stress id is created here

###########################
#However the trend in ATTEMPT 1 appears to be because most of the data is concentrated at data points which correspond to financial stress levels at 2 and 3. Since this is where the most data is, the posteriors appear to be less variant here.
###########################

####These are to check that most points are between 2 and 3
df_comp$fs_id[df_comp$fs_id == 1]
df_comp$fs_id[df_comp$fs_id == 2]
df_comp$fs_id[df_comp$fs_id == 3]
df_comp$fs_id[df_comp$fs_id == 4]

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

#Model 1
#growth = fixed intercept  
          # + b_time[t_id]*debt_lag
          # + b_gr[country_id]*growth_lag
          # + b_debt[country_id]*debt_lag
          # + b_fs[fs_id]*debt_lag
          # + b_lf[country_id]*lf
          # + b_exch[country_id]*exch
          # + b_inter[country_id]*interest
          # + b_inf_rate[country_id]*inf_rate

stanc("Model1_attempt2.stan")$status
fit1 <- stan("Model1_attempt2.stan", 
             data = list("growth",
                         "growth_lag",
                         "debt_lag",
                         "labor",
                        # "fin_stress",
                         "country_id",
                         "exch",
                         "inf_rate",
                         "interest",
                         "t_id",
                         "fs_id"),
             iter = 1000,
             chains = 3)
print(fit1, digits = 3)
lm(growth ~ (growth_lag 
             + debt_lag
             + labor
             + fin_stress
             + exch
             + interest
             + inf_rate))
#######
par(col = "gray")
plot(debt_lag,growth, 
     pch = 16,
     cex = 0.7,
     xlim = range(debt_lag),
     ylim = range(growth),
     col = "white")
points(df_comp$debt_lag[fin_stress < -3], growth[fin_stress < -3],
       pch = 16,
       cex = 0.7,
       col = "lightblue")
points(debt_lag[fin_stress >-3 & fin_stress < -1], 
       growth[fin_stress >-3 & fin_stress < -1],
       pch = 16,
       cex = 0.7,
       col = "pink")
points(debt_lag[fin_stress >-1 & fin_stress < 0], 
       growth[fin_stress >-1 & fin_stress < 0],
       pch = 16,
       cex = 0.7,
       col = "rosybrown")
points(debt_lag[fin_stress > 0 & fin_stress < 2], 
       growth[fin_stress > 0 & fin_stress < 2],
       pch = 16,
       cex = 0.7,
       col = "red")
points(debt_lag[fin_stress > 2 & fin_stress < 4], 
       growth[fin_stress > 2 & fin_stress < 4],
       pch = 16,
       cex = 0.7,
       col = "green")
points(debt_lag[fin_stress > 4 & fin_stress < 8], 
       growth[fin_stress > 4 & fin_stress < 8],
       pch = 16,
       cex = 0.7,
       col = "yellow")
points(debt_lag[fin_stress > 8 & fin_stress < 12], 
       growth[fin_stress > 8 & fin_stress < 12],
       pch = 16,
       cex = 0.7,
       col = "black")
#######

plot(density(x <- rnorm(10^3,0,0.01)))
