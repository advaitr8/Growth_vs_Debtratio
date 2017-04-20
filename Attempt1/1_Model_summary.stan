data{
  real growth[1930];
  real growth_lag[1930];
  real debt_lag[1930];
  real labor[1930];
  real fin_stress[1930];
  int country_id[1930];
}
parameters{
  real beta_0[16];
  real beta_gr[16];
  real beta_d[16];
  real beta_l[16];
  real beta_fs[16];
  real <lower = 0> sigma;
  real mu_0;
  real mu_gr;
  real mu_d;
  real mu_l;
  real mu_fs;
  real <lower = 0> tau_0;
  real <lower = 0> tau_gr;
  real <lower = 0> tau_d;
  real <lower = 0> tau_l;
  real <lower = 0> tau_fs;
}
model{
  for(i in 1:1930){
    growth[i] ~ normal(beta_0[country_id[i]] + 
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_l[country_id[i]]*labor[i] + 
                       beta_fs[country_id[i]]*fin_stress[i],
                       sigma);
  }
  beta_0 ~ normal(mu_0,tau_0);
  beta_gr ~ normal(mu_gr, tau_gr);
  beta_d ~ normal(mu_d, tau_d);
  beta_l ~ normal(mu_l, tau_l);
  beta_fs ~ normal(mu_fs, tau_fs);
  mu_0 ~ normal(0,5);
  tau_0 ~ normal(0,5);
  mu_gr ~ normal(0,5);
  tau_gr ~ normal(0,5);
  mu_d ~ normal(0,5);
  tau_d ~ normal(0,5);
  mu_l ~ normal(0,5);
  tau_l ~ normal(0,5);
  mu_fs ~ normal(0,5);
  tau_fs ~ normal(0,5);
}
generated quantities{
  real growth_pred[1930];
  for(i in 1:1930){
    growth_pred[i] = normal_rng(beta_0[country_id[i]] + 
                                beta_gr[country_id[i]]*growth_lag[i] +
                                beta_d[country_id[i]]*debt_lag[i] +
                                beta_l[country_id[i]]*labor[i] + 
                                beta_fs[country_id[i]]*fin_stress[i],
                                sigma);
  }
  
}
