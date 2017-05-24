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
#However the trend in ATTEMPT 1 appears to be because most of 
# the data is concentrated at data points which correspond to 
# financial stress levels at 2 and 3. Since this is where the 
# most data is, the posteriors appear to be less variant here.
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
#######################################################################

#growth = beta_0 
          # + beta_fs[fs_id[i]]
          # + growth_lag[i] 
          # + beta_debt[country_id[i]]*debt_lag[i] 
          # + beta_labor*labor[i]
          # + beta_exch*exch[i] 
          # + beta_int*int[i]
          # + beta_inf*inf[i]



length(growth)
stanc("model1_attempt3.stan")
fit_1 <- stan("model1_attempt3.stan",
              data = list("growth",
                          "growth_lag",
                          "debt_lag",
                          "labor",
                          "country_id",
                          "exch",
                          "inf_rate",
                          "interest",
                          "fs_id"),
              iter = 1000,
              chains = 3)
print(fit_1, digits = 3)
#include fs_id
#fit_1 is the shit

fit_2 <- stan("model1_attempt3.stan",
              data = list("growth",
                          "growth_lag",
                          "debt_lag",
                          "labor",
                          "country_id",
                          "exch",
                          "inf_rate",
                          "interest",
                          "fs_id"),
              iter = 2000,
              chains = 4)
print(fit_2, digits = 3)
#######################################################################
#####
#Plots
#####

#Using the tidy command to pop out a data frame with results of the Bayesian estimation. Stores mean and 50% posterior interval
tidy_fit_2 <- tidy(fit_2, conf.int = T, conf.level = 0.5)
str(tidy_fit_2)
tidy_fit_2[2:7,]

####
#plot the 6 financial stress coefficients
par(mfrow = c(2, 3),
    mar = c(1.8 , 2.4 , 1, 1),
    cex = 0.6 ,
    las = 1,
    col = "gray")  
for(i in 1:6){
  plot(density(ext2$beta_fs[,i]),
       main = paste("fin stress level",i), 
       col = "black",
       xlim = c(-1,1),
       ylim = c(0,6),
       cex.main = 0.9)
  #
  abline(v = 0, lty = 2, col = "gray")
  abline(v = mean(ext2$beta_fs[,i]), lty = 2, col = "black")
  legend('topleft', 
         legend = c('mean', 'zero'), 
         col = c("black", "gray"), 
         lty = 2, bty = 'n', text.col = "black")
}
#######################################################################
#plot the marginal distribution of beta_debt for 16 countries
ext2 <- extract(fit_2)
par(mfrow = c(4, 4),
    mar = c(1.8 , 2.4 , 1, 1),
    cex = 0.6 ,
    las = 1,
    col = "gray")  
for(i in 1:16){
  plot(density(ext2$beta_debt[,i]),
       main = paste(country_vector[i]), 
       col = "black",
       xlim = c(-0.01,0.01),
       ylim = c(0,400))
  #
  abline(v = 0, lty = 2, col = "gray")
  abline(v = mean(ext2$beta_debt[,i]), lty = 2, col = "black")
  legend('topleft', 
         legend = c('mean', 'zero'), 
         col = c("black", "gray"), 
         lty = 2, bty = 'n', text.col = "black",
         cex = 0.7)
}
#######################################################################
#Posterior predictive checks
df_dupl1 <- df_comp
df_dupl1$pred <- tidy_fit_2$estimate[34:1938]
df_dupl1$lower <- tidy_fit_2$conf.low[34:1938]
df_dupl1$upper <- tidy_fit_2$conf.high[34:1938]

#Plot
par( mfrow = c(4, 4),
     mar = c(2 , 1.7 , 1.2, 1.2),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_dupl1$pred[df_dupl1$country_id == i],
       df_dupl1$growth_rate[df_dupl1$country_id == i],
       pch = 16,
       cex = 0.7,
       main = paste(country_vector[i]),
       cex.main = 0.8,
       col = "white",
       xlim = c(-3,3),
       ylim = c(-3,3))
  #
  arrows ( df_dupl1$pred[df_dupl1$country_id == i] , 
           df_dupl1$growth_rate[df_dupl1$country_id == i],
           df_dupl1$lower[df_dupl1$country_id == i],
           df_dupl1$growth_rate[df_dupl1$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7)
  arrows ( df_dupl1$pred[df_dupl1$country_id == i] , 
           df_dupl1$growth_rate[df_dupl1$country_id == i],
           df_dupl1$upper[df_dupl1$country_id == i],
           df_dupl1$growth_rate[df_dupl1$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7 )
  points(df_dupl1$pred[df_dupl1$country_id == i],
         df_dupl1$growth_rate[df_dupl1$country_id == i],
         pch = 16,
         cex = 0.5,
         col = "black")
  abline(0,1, lty = 2)
  legend ('topleft', legend = c("Data", "50% Interval"),
          col = c("black  " , "darkgrey") , pch=c(16,3),
          lty = c(0, 1),
          bty = 'n',
          cex = 0.7,
          text.col = "black")
}

#######################################################################
#Model 6 including Europe and non Europe countries
#######################################################################
str(df_comp)
############################################################################
#EUROPE
df_eur <- df_comp
#Now pick the European countries
df_eur <- df_eur[df_eur$country_id %in% c(2,3,5,6,7,8,10,11,13,14,15),]
dim(df_eur)
#Only Europe is left.

# #Column of 1's for Europe
# df_eur$indic_eur <- rep(1,1289)
# df_eur$eur_debt <- df_eur$debt_lag*df_eur$indic_eur
# #Column of 0's for Non Europe
# df_eur$indic_non_eur <- rep(0,1289)
# df_eur$non_eur_debt <- df_eur$debt_lag*df_eur$indic_non_eur
# ##
eur_debt <- df_eur$debt_lag
length(unique(df_eur$country_id))
#Model 6 for Europe
growth <- df_eur$growth_rate
growth_lag <- df_eur$growth_lag
debt_lag <- df_eur$debt_lag
labor <- df_eur$labor
fin_stress <- df_eur$fin_stress
country_id <- df_eur$country_id
t_id <- df_eur$t_id
#a little hack here for good plots
for(i in 1:1905){
  if(t_id[i] == 33){
    t_id[i] = 32}
}
#I make all time period 33 into time period 32
exch <- df_eur$exch
interest <- df_eur$interest
inf_rate <- df_eur$inf_rate
fs_id <- df_eur$fs_id


stanc("model2_attempt3.stan")
fit_3 <- stan("model2_attempt3.stan",
              data = list("growth",
                          "growth_lag",
                          "eur_debt",
                          "labor",
                          "country_id",
                          "exch",
                          "inf_rate",
                          "interest",
                          "fs_id"),
              iter = 1000,
              chains = 3)
print(fit_3, digits = 3)


############################################################################
#NONEUROPE
df_non_eur <- df_comp
#Now pick the Non - European countries
df_non_eur <- df_non_eur[df_non_eur$country_id %in% c(1,4,9,12,16),]
dim(df_non_eur)
#Only Non Europe is left.

#Column of 0's for Europe
df_non_eur$indic_eur <- rep(0,616)
df_non_eur$eur_debt <- df_non_eur$debt_lag*df_non_eur$indic_eur
#Column of 1's for Non Europe
df_non_eur$indic_non_eur <- rep(1,616)
df_non_eur$non_eur_debt <- df_non_eur$debt_lag*df_non_eur$indic_non_eur

#####

#Model 6 where European and Non European countries are separated




