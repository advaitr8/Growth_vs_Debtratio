#rm(list = ls())
setwd("/Users/Advait/Desktop/New_School/Christian.paper/Code")

source("data_munging.R")
str(df_comp)
#######################################################################
#Exploratory data analysis
#######################################################################

#Plot 1 - Growth Rate vs Debt Ratio Lagged
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_comp$debt_lag[df_comp$country_id == i], 
       df_comp$growth_rate[df_comp$country_id == i], 
       cex = 0.7, pch = 16,
       xlab = "debt_lag",
       ylab = "growth_rate",
       main = paste(country_vector[i]),
       cex.main = 0.8,
       ylim = c(-4,4),
       col = "black")
  abline(lm(df_comp$growth_rate[df_comp$country_id == i] ~
              df_comp$debt_lag[df_comp$country_id == i]),
         col = "red")
}
####
#Plot 2 - Growth Rate vs Financial Stress
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_comp$fin_stress[df_comp$country_id == i], 
       df_comp$growth_rate[df_comp$country_id == i], 
       cex = 0.7, pch = 16,
       xlab = "debt_lag",
       ylab = "growth_rate",
       main = paste(country_vector[i]),
       cex.main = 0.8,
       ylim = c(-4,4),
       xlim = c(-5,17),
       col = "black")
 abline(lm(df_comp$growth_rate[df_comp$country_id == i] ~
              df_comp$fin_stress[df_comp$country_id == i]),
         col = "red")
}
####
#Plot 3 - Growth vs Labor force growth
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_comp$labor[df_comp$country_id == i], 
       df_comp$growth_rate[df_comp$country_id == i], 
       cex = 0.7, pch = 16,
       xlab = "labor",
       ylab = "growth_rate",
       main = paste(country_vector[i]),
       cex.main = 0.8,
       ylim = c(-4,4),
       xlim = c(-2,2),
       col = "black")
  abline(lm(df_comp$labor[df_comp$country_id == i] ~
              df_comp$growth_rate[df_comp$country_id == i]),
         col = "red")
}

####
#Plot 4 - Growth vs debt (fin stress two thresholds)
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_comp$debt_lag[df_comp$country_id == i], 
       df_comp$growth_rate[df_comp$country_id == i], 
       cex = 0.7, pch = 16,
       xlab = "debt_lag",
       ylab = "growth_rate",
       main = paste(country_vector[i]),
       cex.main = 0.8,
       ylim = c(-4,4),
       col = "gray")
  abline(lm(df_comp$growth_rate[df_comp$country_id == i] ~
              df_comp$debt_lag[df_comp$country_id == i]),
         col = "red")
  points(df_comp$debt_lag[df_comp$country_id == i &
                          df_comp$fin_stress > 2 &
                          df_comp$fin_stress < 6],
         df_comp$growth_rate[df_comp$country_id == i &
                          df_comp$fin_stress > 2 &
                          df_comp$fin_stress < 6],
         col = "black",
         pch = 16,
         cex = 0.7)
}

#######################################################################
#Stan models
#######################################################################
growth <- df_comp$growth_rate
growth_lag <- df_comp$growth_lag
debt_lag <- df_comp$debt_lag
labor <- df_comp$labor
fin_stress <- df_comp$fin_stress
country_id <- df_comp$country_id
###
#Model 1
#growth = country_int + growth_lag + debt_gdp_lag + fin_stress + labor
stanc("1_Model_summary.stan")
fit_1 <- stan("1_Model_summary.stan", data = list("growth",
                                                  "growth_lag",
                                                  "debt_lag",
                                                  "labor",
                                                  "fin_stress",
                                                  "country_id"),
              iter = 1000, 
              chains = 3)

tidy_fit_1 <- tidy(fit_1, conf.int = T, conf.level = 0.5)
str(tidy_fit_1)
ext1 <- extract(fit_1)
print(fit_1, digits = 3)
##
#Posterior predictive checks
df_dupl1 <- df_comp
df_dupl1$pred <- tidy_fit_1$estimate[92:2021]
df_dupl1$lower <- tidy_fit_1$conf.low[92:2021]
df_dupl1$upper <- tidy_fit_1$conf.high[92:2021]

#Plot
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
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
#Model 2 Data manipulation
#growth = country_int + growth_lag + low_thresh_int(2.27) + high_thresh_int(2.27) 
# +debt_gdp_lag + labor
###
df_dupl2 <- df_comp
str(df_dupl2)
#creating indicator
df_dupl2$regime_hi <- ifelse(df_dupl2$fin_stress > 2.27, 1 , 0)
df_dupl2$regime_lo <- ifelse(df_dupl2$fin_stress < 2.27, 1 , 0)
df_dupl2$debt_lag_hi <- df_dupl2$debt_lag*df_dupl2$regime_hi
df_dupl2$debt_lag_lo <- df_dupl2$debt_lag*df_dupl2$regime_lo
##
#Running model 2
growth <- df_dupl2$growth_rate
growth_lag <- df_dupl2$growth_lag
debt_lag_lo <- df_dupl2$debt_lag_lo
debt_lag_hi <- df_dupl2$debt_lag_hi
regime_lo <- df_dupl2$regime_lo
regime_hi <- df_dupl2$regime_hi
labor <- df_dupl2$labor
country_id <- df_dupl2$country_id

stanc("2_Model_summary.stan")
fit_2 <- stan("2_Model_summary.stan",
              data = list("growth",
                          "growth_lag",
                          "debt_lag_lo",
                          "debt_lag_hi",
                        #  "regime_lo",
                          "regime_hi",
                          "labor",
                          "country_id"), 
              iter = 1000,
              chains = 3)

tidy_fit_2 <- tidy(fit_2, conf.int = T, conf.level = 0.5)
str(tidy_fit_2)
ext2 <- extract(fit_2)
print(fit_2, digits = 3)
#Posterior predictive checks
df_dupl2$pred <- tidy_fit_2$estimate[95:2024]
df_dupl2$lower <- tidy_fit_2$conf.low[95:2024]
df_dupl2$upper <- tidy_fit_2$conf.high[95:2024]

#Plot
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_dupl2$pred[df_dupl2$country_id == i],
       df_dupl2$growth_rate[df_dupl2$country_id == i],
       pch = 16,
       cex = 0.7,
       main = paste(country_vector[i]),
       cex.main = 0.8,
       col = "white",
       xlim = c(-3,3),
       ylim = c(-3,3))
  #
  arrows ( df_dupl2$pred[df_dupl2$country_id == i] , 
           df_dupl2$growth_rate[df_dupl2$country_id == i],
           df_dupl2$lower[df_dupl2$country_id == i],
           df_dupl2$growth_rate[df_dupl2$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7)
  arrows ( df_dupl2$pred[df_dupl2$country_id == i] , 
           df_dupl2$growth_rate[df_dupl2$country_id == i],
           df_dupl2$upper[df_dupl2$country_id == i],
           df_dupl2$growth_rate[df_dupl2$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7 )
  points(df_dupl2$pred[df_dupl2$country_id == i],
         df_dupl2$growth_rate[df_dupl2$country_id == i],
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
#Model 3 Data manipulation
#growth = country_int + debt_gdp + labor
#WHERE debt_gdp = f(fin stress)
# df_dupl3 <- df_comp
# str(df_comp)
# 
# growth <- df_dupl3$growth_rate
# growth_lag <- df_dupl3$growth_lag
# debt_lag <- df_dupl3$debt_lag
# labor <- df_dupl3$labor
# fin_stress <- df_dupl3$fin_stress
# country_id <- df_dupl3$country_id
# 
# stanc("3_Model_summary.stan")
# 
# fit_3 <- stan("3_Model_summary.stan",
#               data = list("growth",
#                           "growth_lag",
#                           "debt_lag",
#                           "labor",
#                           "fin_stress",
#                           "country_id"),
#               iter = 1000,
#               chains = 3)
# print(fit_3, digits = 4)

#This model don't work.

#######################################################################
#Model 4
#growth = country_int + growth_lag + debt_all_lag + debt (0,2,6,10) + finstress
df_dupl4 <- df_comp
df_dupl4$d_indic_0 <- ifelse(df_dupl4$fin_stress > 0, 1 , 0)
df_dupl4$d_indic_2 <- ifelse(df_dupl4$fin_stress > 2, 1 , 0)
df_dupl4$d_indic_6 <- ifelse(df_dupl4$fin_stress > 6, 1 , 0)
df_dupl4$d_indic_10 <- ifelse(df_dupl4$fin_stress > 9, 1 , 0)
#
df_dupl4$d_0 <- df_dupl4$d_indic_0*df_dupl4$debt_lag
df_dupl4$d_2 <- df_dupl4$d_indic_2*df_dupl4$debt_lag
df_dupl4$d_6 <- df_dupl4$d_indic_6*df_dupl4$debt_lag
df_dupl4$d_10 <- df_dupl4$d_indic_10*df_dupl4$debt_lag

#setting up the data for Stan
growth <- df_dupl4$growth_rate
growth_lag <- df_dupl4$growth_lag
debt_lag <- df_dupl4$debt_lag
d_0 <- df_dupl4$d_0
d_2 <- df_dupl4$d_2
d_6 <- df_dupl4$d_6
d_10 <- df_dupl4$d_0
labor <- df_dupl4$labor
fin_stress <- df_dupl4$fin_stress
country_id <- df_dupl4$country_id

#run model
stanc("4_Model_summary.stan")

fit_4 <- stan("4_Model_summary.stan",
              data = list("growth",
                          "growth_lag",
                          "debt_lag",
                          "d_0",
                          "d_2",
                          "d_6",
                          "d_10",
                          "labor",
                          "fin_stress",
                          "country_id"),
              iter = 1000,
              chains = 3)


tidy_fit_4 <- tidy(fit_4, conf.int = T, conf.level = 0.5)
str(tidy_fit_4)
ext4 <- extract(fit_4)
print(fit_4, digits = 3)
dim(tidy_fit_4)

#Posterior predictive checks
df_dupl4$pred <- tidy_fit_4$estimate[164:2093]
df_dupl4$lower <- tidy_fit_4$conf.low[164:2093]
df_dupl4$upper <- tidy_fit_4$conf.high[164:2093]

#Plot
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_dupl4$pred[df_dupl4$country_id == i],
       df_dupl4$growth_rate[df_dupl4$country_id == i],
       pch = 16,
       cex = 0.7,
       main = paste(country_vector[i]),
       cex.main = 0.8,
       col = "white",
       xlim = c(-3,3),
       ylim = c(-3,3))
  #
  arrows ( df_dupl4$pred[df_dupl4$country_id == i] , 
           df_dupl4$growth_rate[df_dupl4$country_id == i],
           df_dupl4$lower[df_dupl4$country_id == i],
           df_dupl4$growth_rate[df_dupl4$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7)
  arrows ( df_dupl4$pred[df_dupl4$country_id == i] , 
           df_dupl4$growth_rate[df_dupl4$country_id == i],
           df_dupl4$upper[df_dupl4$country_id == i],
           df_dupl4$growth_rate[df_dupl4$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7 )
  points(df_dupl4$pred[df_dupl4$country_id == i],
         df_dupl4$growth_rate[df_dupl4$country_id == i],
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

#Beta's plot
par(mfrow = c(4, 4),
    mar = c(1.8 , 2.4 , 1, 1),
    cex = 0.6 ,
    las = 1,
    col = "gray")  
for(i in 1:16){
  plot(density(ext4$beta_d[,i]),
       main = paste(country_vector[i]), 
       col = "black",
       xlim = c(-0.05,0.05),
       ylim = c(0,300))
  #
  abline(v = 0, lty = 2, col = "gray")
  lines(density(ext4$beta_d0[,i]), col = "red")
  lines(density(ext4$beta_d2[,i]), col = "blue")
  lines(density(ext4$beta_d6[,i]), col = "orange")
  lines(density(ext4$beta_d10[,i]), col = "darkgreen")
  #
  legend('topleft', legend = c("d_gdp", 
                               "d_gdp (fs > 0) ",
                               "d_gdp (fs > 2)",
                               "d_gdp (fs > 6)",
                               "d_gdp (fs > 10)"),
         cex = 0.7, col = c("black",
                            "red",
                            "blue",
                            "orange",
                            "darkgreen"), 
         lty = 1, 
         bty = 'n',
         text.col = "black")
}

#######################################################################
#Model 5
#growth = country_int + growth_lag + debt_all_lag + debt (2,3,4,5) + finstress
df_dupl5 <- df_comp
df_dupl5$d_indic_2 <- ifelse(df_dupl5$fin_stress > 2, 1 , 0)
df_dupl5$d_indic_3 <- ifelse(df_dupl5$fin_stress > 3, 1 , 0)
df_dupl5$d_indic_4 <- ifelse(df_dupl5$fin_stress > 4, 1 , 0)
df_dupl5$d_indic_5 <- ifelse(df_dupl5$fin_stress > 5, 1 , 0)
#
df_dupl5$d_2 <- df_dupl5$d_indic_2*df_dupl5$debt_lag
df_dupl5$d_3 <- df_dupl5$d_indic_3*df_dupl5$debt_lag
df_dupl5$d_4 <- df_dupl5$d_indic_4*df_dupl5$debt_lag
df_dupl5$d_5 <- df_dupl5$d_indic_5*df_dupl5$debt_lag

#setting up the data for Stan
growth <- df_dupl5$growth_rate
growth_lag <- df_dupl5$growth_lag
debt_lag <- df_dupl5$debt_lag
d_2 <- df_dupl5$d_2
d_3 <- df_dupl5$d_3
d_4 <- df_dupl5$d_4
d_5 <- df_dupl5$d_5
labor <- df_dupl5$labor
fin_stress <- df_dupl5$fin_stress
country_id <- df_dupl5$country_id

#run model
stanc("5_Model_summary.stan")

fit_5 <- stan("5_Model_summary.stan",
              data = list("growth",
                          "growth_lag",
                          "debt_lag",
                          "d_2",
                          "d_3",
                          "d_4",
                          "d_5",
                          "labor",
                          "fin_stress",
                          "country_id"),
              iter = 1000,
              chains = 3)

#print(fit_5)
tidy_fit_5 <- tidy(fit_5, conf.int = T, conf.level = 0.5)
str(tidy_fit_5)
ext5 <- extract(fit_5)
#print(fit_4, digits = 3)
dim(tidy_fit_5)

#Posterior predictive checks
df_dupl5$pred <- tidy_fit_5$estimate[164:2093]
df_dupl5$lower <- tidy_fit_5$conf.low[164:2093]
df_dupl5$upper <- tidy_fit_5$conf.high[164:2093]



#Plot
par( mfrow = c(4, 4),
     mar = c(1.8 , 1.5 , 1, 1),
     cex = 0.6 ,
     las = 1,
     col = "gray")
for(i in 1:16){
  plot(df_dupl5$pred[df_dupl5$country_id == i],
       df_dupl5$growth_rate[df_dupl5$country_id == i],
       pch = 16,
       cex = 0.7,
       main = paste(country_vector[i]),
       cex.main = 0.8,
       col = "white",
       xlim = c(-3,3),
       ylim = c(-3,3))
  #
  arrows ( df_dupl5$pred[df_dupl5$country_id == i] , 
           df_dupl5$growth_rate[df_dupl5$country_id == i],
           df_dupl5$lower[df_dupl5$country_id == i],
           df_dupl5$growth_rate[df_dupl5$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7)
  arrows ( df_dupl5$pred[df_dupl5$country_id == i] , 
           df_dupl5$growth_rate[df_dupl5$country_id == i],
           df_dupl5$upper[df_dupl5$country_id == i],
           df_dupl5$growth_rate[df_dupl5$country_id == i]  ,
           col = " gray ", length = 0, lwd = 0.7 )
  points(df_dupl5$pred[df_dupl5$country_id == i],
         df_dupl5$growth_rate[df_dupl5$country_id == i],
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

#Beta's plot
par(mfrow = c(4, 4),
    mar = c(1.8 , 2.4 , 1, 1),
    cex = 0.6 ,
    las = 1,
    col = "gray")  
for(i in 1:16){
  plot(density(ext5$beta_d[,i]),
       main = paste(country_vector[i]), 
       col = "black",
       xlim = c(-0.05,0.05),
       ylim = c(0,300))
  #
  abline(v = 0, lty = 2, col = "gray")
  lines(density(ext5$beta_d2[,i]), col = "red")
  lines(density(ext5$beta_d3[,i]), col = "blue")
  lines(density(ext5$beta_d4[,i]), col = "orange")
  lines(density(ext5$beta_d5[,i]), col = "darkgreen")
  #
  legend('topleft', legend = c("d_gdp", 
                               "d_gdp (fs > 2) ",
                               "d_gdp (fs > 3)",
                               "d_gdp (fs > 4)",
                               "d_gdp (fs > 5)"),
         cex = 0.7, col = c("black",
                            "red",
                            "blue",
                            "orange",
                            "darkgreen"), 
         lty = 1, 
         bty = 'n',
         text.col = "black")
}
