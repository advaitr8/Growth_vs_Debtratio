data{
  real growth[1930];
  real growth_lag[1930];
  real debt_lag[1930];
  real d_0[1930];
  real d_2[1930];
  real d_6[1930];
  real d_10[1930];
  real labor[1930];
  real fin_stress[1930];
  int country_id[1930];
}
parameters{
  real beta_0[16];
  real beta_gr[16];
  real beta_d[16];
  real beta_d0[16];
  real beta_d2[16];
  real beta_d6[16];
  real beta_d10[16];
  real beta_l[16];
  real beta_fs[16];
  real <lower = 0> sigma;
  real mu_0;
  real <lower = 0> tau_0;
  real mu_gr;
  real <lower = 0> tau_gr;
  real mu_d;
  real <lower = 0> tau_d;
  real mu_d0;
  real <lower = 0> tau_d0;
  real mu_d2;
  real <lower = 0> tau_d2;
  real mu_d6;
  real <lower = 0> tau_d6;
  real mu_d10;
  real <lower = 0> tau_d10;
  real mu_l;
  real <lower = 0> tau_l;
  real mu_fs;
  real <lower = 0> tau_fs;
  
}
model{
  for (i in 1:1930){
    growth[i] ~ normal(beta_0[country_id[i]] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_d0[country_id[i]]*d_0[i] +
                       beta_d2[country_id[i]]*d_2[i] +
                       beta_d6[country_id[i]]*d_6[i] +
                       beta_d10[country_id[i]]*d_10[i] +
                       beta_l[country_id[i]]*labor[i] +
                       beta_fs[country_id[i]]*fin_stress[i],
                       sigma);
  }
  beta_0 ~ normal(mu_0,tau_0);
  beta_gr ~ normal(mu_gr, tau_gr);
  beta_d ~ normal(mu_d, tau_d);
  beta_d0 ~ normal(mu_d0, tau_d0);
  beta_d2 ~ normal(mu_d2, tau_d2);
  beta_d6 ~ normal(mu_d0, tau_d6);
  beta_d10 ~ normal(mu_d10, tau_d10);
  beta_l ~ normal(mu_l, tau_l);
  beta_fs ~ normal(mu_fs, tau_fs);
  mu_0 ~ normal(0,3);
  tau_0 ~ normal(0,3);
  mu_gr ~ normal(0,3);
  tau_gr ~ normal(0,3);
  mu_d ~ normal(0,3);
  tau_d ~ normal(0,3);
  mu_d0 ~ normal(0,3);
  tau_d0 ~ normal(0,3);
  mu_d2 ~ normal(0,3);
  tau_d2 ~ normal(0,3);
  mu_d6 ~ normal(0,3);
  tau_d6 ~ normal(0,3);
  mu_d10 ~ normal(0,3);
  tau_d10 ~ normal(0,3);
  mu_l ~ normal(0,3);
  tau_l ~ normal(0,3);
  mu_fs ~ normal(0,3);
  tau_fs ~ normal(0,3);
  
}
generated quantities{
  real pred[1930];
  for (i in 1:1930){
    pred[i] = normal_rng(beta_0[country_id[i]] +
                       beta_gr[country_id[i]]*growth_lag[i] +
                       beta_d[country_id[i]]*debt_lag[i] +
                       beta_d0[country_id[i]]*d_0[i] +
                       beta_d2[country_id[i]]*d_2[i] +
                       beta_d6[country_id[i]]*d_6[i] +
                       beta_d10[country_id[i]]*d_10[i] +
                       beta_l[country_id[i]]*labor[i] +
                       beta_fs[country_id[i]]*fin_stress[i],
                       sigma);
  }
  
}
